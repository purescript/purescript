{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

module Language.PureScript.Interactive
  ( handleCommand
  , module Interactive

  -- TODO: remove these exports
  , make
  , runMake
  ) where

import           Prelude ()
import           Prelude.Compat

import           Data.List (intercalate, nub, sort, find, foldl')
import           Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Foldable (fold)

import           Control.Applicative ((<|>))
import           Control.Arrow ((>>>), first, second)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Class
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.State.Strict (StateT, runStateT)
import           Control.Monad.Writer.Strict (Writer(), runWriter)

import qualified Language.PureScript as P
import qualified Language.PureScript.Names as N

import           Language.PureScript.Interactive.Completion   as Interactive
import           Language.PureScript.Interactive.IO           as Interactive
import           Language.PureScript.Interactive.Message      as Interactive
import           Language.PureScript.Interactive.Module       as Interactive
import           Language.PureScript.Interactive.Parser       as Interactive
import           Language.PureScript.Interactive.Printer      as Interactive
import           Language.PureScript.Interactive.Types        as Interactive

import           System.FilePath ((</>))

-- | Pretty-print errors
printErrors :: MonadIO m => P.MultipleErrors -> m ()
printErrors = liftIO . putStrLn . P.prettyPrintMultipleErrors P.defaultPPEOptions

-- | This is different than the runMake in 'Language.PureScript.Make' in that it specifies the
-- options and ignores the warning messages.
runMake :: P.Make a -> IO (Either P.MultipleErrors a)
runMake mk = fst <$> P.runMake P.defaultOptions mk

-- | Rebuild a module, using the cached externs data for dependencies.
rebuild
  :: [P.ExternsFile]
  -> P.Module
  -> P.Make (P.ExternsFile, P.Environment)
rebuild loadedExterns m = do
    externs <- P.rebuildModule buildActions loadedExterns m
    return (externs, foldl' (flip P.applyExternsFileToEnvironment) P.initEnvironment (loadedExterns ++ [externs]))
  where
    buildActions :: P.MakeActions P.Make
    buildActions =
      (P.buildMakeActions modulesDir
                          filePathMap
                          M.empty
                          False) { P.progress = const (return ()) }

    filePathMap :: M.Map P.ModuleName (Either P.RebuildPolicy FilePath)
    filePathMap = M.singleton (P.getModuleName m) (Left P.RebuildAlways)

-- | Build the collection of modules from scratch. This is usually done on startup.
make
  :: [(FilePath, P.Module)]
  -> P.Make ([P.ExternsFile], P.Environment)
make ms = do
    foreignFiles <- P.inferForeignModules filePathMap
    externs <- P.make (buildActions foreignFiles) (map snd ms)
    return (externs, foldl' (flip P.applyExternsFileToEnvironment) P.initEnvironment externs)
  where
    buildActions :: M.Map P.ModuleName FilePath -> P.MakeActions P.Make
    buildActions foreignFiles =
      P.buildMakeActions modulesDir
                         filePathMap
                         foreignFiles
                         False

    filePathMap :: M.Map P.ModuleName (Either P.RebuildPolicy FilePath)
    filePathMap = M.fromList $ map (\(fp, m) -> (P.getModuleName m, Right fp)) ms

-- | Performs a PSCi command
handleCommand
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => (String -> m ())
  -> m ()
  -> Command
  -> m ()
handleCommand _ _ ShowHelp                  = liftIO $ putStrLn helpMessage
handleCommand _ r ResetState                = handleResetState r
handleCommand c _ (Expression val)          = handleExpression c val
handleCommand _ _ (Import im)               = handleImport im
handleCommand _ _ (Decls l)                 = handleDecls l
handleCommand _ _ (TypeOf val)              = handleTypeOf val
handleCommand _ _ (InfoFor ident)           = handleInfoFor ident
handleCommand _ _ (KindOf typ)              = handleKindOf typ
handleCommand _ _ (BrowseModule moduleName) = handleBrowse moduleName
handleCommand _ _ (ShowInfo QueryLoaded)    = handleShowLoadedModules
handleCommand _ _ (ShowInfo QueryImport)    = handleShowImportedModules
handleCommand _ _ _                         = P.internalError "handleCommand: unexpected command"

-- | Reset the application state
handleResetState
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => m ()
  -> m ()
handleResetState reload = do
  modify $ updateImportedModules (const [])
         . updateLets (const [])
  files <- asks psciLoadedFiles
  e <- runExceptT $ do
    modules <- ExceptT . liftIO $ loadAllModules files
    (externs, _) <- ExceptT . liftIO . runMake . make $ modules
    return (map snd modules, externs)
  case e of
    Left errs -> printErrors errs
    Right (modules, externs) -> do
      modify (updateLoadedExterns (const (zip modules externs)))
      reload

-- | Takes a value expression and evaluates it with the current state.
handleExpression
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => (String -> m ())
  -> P.Expr
  -> m ()
handleExpression evaluate val = do
  st <- get
  let m = createTemporaryModule True st val
  e <- liftIO . runMake $ rebuild (map snd (psciLoadedExterns st)) m
  case e of
    Left errs -> printErrors errs
    Right _ -> do
      js <- liftIO $ readFile (modulesDir </> tempModuleNameRaw </> "index.js")
      evaluate js

-- |
-- Takes a list of declarations and updates the environment, then run a make. If the declaration fails,
-- restore the original environment.
--
handleDecls
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => [P.Declaration]
  -> m ()
handleDecls ds = do
  st <- gets (updateLets (++ ds))
  let m = createTemporaryModule False st (P.Literal (P.ObjectLiteral []))
  e <- liftIO . runMake $ rebuild (map snd (psciLoadedExterns st)) m
  case e of
    Left err -> printErrors err
    Right _ -> put st

-- | Show actual loaded modules in psci.
handleShowLoadedModules
  :: (MonadState PSCiState m, MonadIO m)
  => m ()
handleShowLoadedModules = do
    loadedModules <- gets psciLoadedExterns
    liftIO $ putStrLn (readModules loadedModules)
  where
    readModules = unlines . sort . nub . map (P.runModuleName . P.getModuleName . fst)

-- | Show the imported modules in psci.
handleShowImportedModules
  :: (MonadState PSCiState m, MonadIO m)
  => m ()
handleShowImportedModules = do
  PSCiState { psciImportedModules = importedModules } <- get
  liftIO $ showModules importedModules >>= putStrLn
  return ()
  where
  showModules = return . unlines . sort . map showModule
  showModule (mn, declType, asQ) =
    "import " ++ N.runModuleName mn ++ showDeclType declType ++
    foldMap (\mn' -> " as " ++ N.runModuleName mn') asQ

  showDeclType P.Implicit = ""
  showDeclType (P.Explicit refs) = refsList refs
  showDeclType (P.Hiding refs) = " hiding " ++ refsList refs
  refsList refs = " (" ++ commaList (mapMaybe showRef refs) ++ ")"

  showRef :: P.DeclarationRef -> Maybe String
  showRef (P.TypeRef pn dctors) =
    Just $ N.runProperName pn ++ "(" ++ maybe ".." (commaList . map N.runProperName) dctors ++ ")"
  showRef (P.TypeOpRef op) =
    Just $ "type " ++ N.showOp op
  showRef (P.ValueRef ident) =
    Just $ N.runIdent ident
  showRef (P.ValueOpRef op) =
    Just $ N.showOp op
  showRef (P.TypeClassRef pn) =
    Just $ "class " ++ N.runProperName pn
  showRef (P.TypeInstanceRef ident) =
    Just $ N.runIdent ident
  showRef (P.ModuleRef name) =
    Just $ "module " ++ N.runModuleName name
  showRef (P.ReExportRef _ _) =
    Nothing
  showRef (P.PositionedDeclarationRef _ _ ref) =
    showRef ref

  commaList :: [String] -> String
  commaList = intercalate ", "

-- | Imports a module, preserving the initial state on failure.
handleImport
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => ImportedModule
  -> m ()
handleImport im = do
   st <- gets (updateImportedModules (im :))
   let m = createTemporaryModuleForImports st
   e <- liftIO . runMake $ rebuild (map snd (psciLoadedExterns st)) m
   case e of
     Left errs -> printErrors errs
     Right _  -> put st

-- | Takes a value and prints its type
handleTypeOf
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => P.Expr
  -> m ()
handleTypeOf val = do
  st <- get
  let m = createTemporaryModule False st val
  e <- liftIO . runMake $ rebuild (map snd (psciLoadedExterns st)) m
  case e of
    Left errs -> printErrors errs
    Right (_, env') ->
      case
        M.lookup
          (P.mkQualified
            (P.Ident "it")
            tempModuleName)
          (P.names env')
      of
        Just (ty, _, _) -> liftIO . putStrLn . P.prettyPrintType $ ty
        Nothing -> liftIO $ putStrLn "Could not find type"

-- | Takes a type and prints its kind
handleKindOf
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => P.Type
  -> m ()
handleKindOf typ = do
  st <- get
  let m = createTemporaryModuleForKind st typ
      mName = tempModuleName
  e <- liftIO . runMake $ rebuild (map snd (psciLoadedExterns st)) m
  case e of
    Left errs -> printErrors errs
    Right (_, env') ->
      case M.lookup (P.Qualified (Just mName) $ P.ProperName "IT") (P.typeSynonyms env') of
        Just (_, typ') -> do
          let chk = (P.emptyCheckState env') { P.checkCurrentModule = Just mName }
              k   = check (P.kindOf typ') chk

              check :: StateT P.CheckState (ExceptT P.MultipleErrors (Writer P.MultipleErrors)) a -> P.CheckState -> Either P.MultipleErrors (a, P.CheckState)
              check sew = fst . runWriter . runExceptT . runStateT sew
          case k of
            Left err        -> printErrors err
            Right (kind, _) -> liftIO . putStrLn . P.prettyPrintKind $ kind
        Nothing -> liftIO $ putStrLn "Could not find kind"

-- | The data for :info. Could be a class, a data constructor, a type or a value
-- 
data InfoPrintable = InfoPrintable
  (P.Qualified String)
  -- ^ The name of this item
  InfoTarget
  -- ^ Data specific to the item

data InfoType =
  InfoType
    P.DataDeclType
    (P.ProperName 'N.TypeName)
    [(String, Maybe P.Kind)]

data InfoConstructor =
  InfoConstructor
    P.Type
    -- ^ Type of constructor (as displayed in GADT form)
    [P.Type]
    -- ^ Arguments to constructor (as displayed in classic form)

data InfoTarget
  = IValue
      P.Type
  | IConstructor
      InfoType
      InfoConstructor
  | IType
    InfoType
    [(String, InfoConstructor)]
  | ITypeSynonym
    [(String, Maybe P.Kind)]
    P.Type
  | ITypeClass
      P.TypeClassData
      [(Maybe P.ModuleName, P.ProperName 'N.TypeName)]

data InfoCtors
  = Single (String, InfoConstructor)
  | All [(String, InfoConstructor)]

showInfo :: InfoPrintable -> String
showInfo (InfoPrintable idn@(P.Qualified mMdl nm) target) =
  case target of
    IValue typ ->
      defCmt mMdl
      ++ "\n"
      ++ nm
      ++ " :: "
      ++ tyInline typ
    IConstructor tyI ty ->
      showAdt mMdl tyI $ Single (nm, ty)
    IType        iTy ctors ->
      showAdt mMdl iTy $ All ctors
    ITypeSynonym args ty ->
      fold
        [ defCmt mMdl
        , "\n"
        , "type "
        , nm
        , foldMap ((" " ++) . showTypeVar) args
        , " = "
        , tyInline ty
        ]
    ITypeClass   _ _ -> undefined
  where
    stripStr = T.unpack . T.strip . T.pack
    tyInline = stripStr . P.prettyPrintType

    defCmt mMod =
      maybe
        "-- Defined interactively"
        (("-- Defined in " ++) . N.runModuleName)
        mMod

    showTypeVar (nm, Nothing) =
      nm
    showTypeVar (nm, Just knd) =
      "(" ++ nm ++ " :: " ++ P.prettyPrintKind knd ++ ")"

    showAdt
      :: Maybe P.ModuleName
      -> InfoType
      -> InfoCtors
      -> String
    showAdt mMdl (InfoType P.Data tyName vars) ctors =
      fold
        [ defCmt mMdl
        , "\n"
        , "data "
        , P.runProperName tyName
        , foldMap ((" " ++) . showTypeVar) vars
        , " where"
        , showGADTCtors ctors
        ]

    -- TODO: Allow showing with both syntaxes
    showAdt mMdl (InfoType P.Newtype tyName vars) (Single ct) =
      showNewtype mMdl tyName vars [ct]
    -- Defined so that showing the newtype by type name doesn't explode
    showAdt mMdl (InfoType P.Newtype tyName vars) (All cts) =
      showNewtype mMdl tyName vars cts

    showNewtype mMdl tyName vars cts =
      fold
        [ defCmt mMdl
        , "\n"
        , "newtype "
        , P.runProperName tyName
        , foldMap ((" " ++) . showTypeVar) vars
        , " = "
        , foldMap showCtor cts
        ]

    showGADTCtors (Single c) = "\n  ...\n  " ++ showCtor c ++ "\n  ..."
    showGADTCtors (All cs) = foldMap (("\n  " ++) . showGADTCtor) cs

    showGADTCtor :: (String, InfoConstructor) -> String
    showGADTCtor (n, InfoConstructor t _) = n ++ " :: " ++ tyInline t

    showCtor :: (String, InfoConstructor) -> String
    showCtor (n, InfoConstructor _ ts) =
      n ++ foldMap ((" " ++) . tyInline) ts

-- | Takes a type and prints info about it
handleInfoFor
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => P.Qualified String
  -> m ()
handleInfoFor ident@(P.Qualified mMod name) = do
  st <- get
  let tMod = createTemporaryModuleForInfo st
      ident' =
        P.mkQualified name $
          fromMaybe tempModuleName mMod
  e <- liftIO . runMake $ rebuild (map snd (psciLoadedExterns st)) tMod
  case e of
    Left errs -> printErrors errs
    Right (_, env') ->
      liftIO
      . putStrLn
      . maybe ("Not in scope: " ++ N.showQualified id ident) showInfo
      . fmap (InfoPrintable ident)
      . firstJust
      . map (\f -> f env' ident')
      $ [ getPrintableFromNames
        , getPrintableFromTypeclasses
        , getPrintableFromTypes
        , getPrintableFromConstructors
        ]

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr (<|>) Nothing

type GetPrintable = P.Environment -> P.Qualified String -> Maybe InfoTarget

getPrintableFromNames :: GetPrintable
getPrintableFromNames env qul = go <$> M.lookup (P.Ident <$> qul) (P.names env)
  where
    go (ty, _, _) = IValue ty

getPrintableFromTypeclasses :: GetPrintable
getPrintableFromTypeclasses = const . const $ Nothing

getPrintableFromTypes :: GetPrintable
getPrintableFromTypes env qul@(P.Qualified _ unQul) =
  M.lookup (P.ProperName <$> qul) (P.types env) >>= go
  where
    ctorInfo
      :: P.ProperName 'N.ConstructorName
      -> [P.Type]
      -> Maybe (P.DataDeclType, (String, InfoConstructor))
    ctorInfo nm argTypes =
      go' <$> M.lookup (const nm <$> qul) (P.dataConstructors env)
      where
        go' (ddt, _, ty, _) =
          (ddt, (P.runProperName nm, InfoConstructor ty argTypes))
    go
      :: (P.Kind, P.TypeKind)
      -> Maybe InfoTarget
    go (knd, P.DataType vars ctors) =
      makeTypeInfo vars <$>
        -- Returns Just [a] if all elements are Just, Nothing otherwise
        foldr
          (\c l -> (:) <$> c <*> l)
          (Just [])
          (map (uncurry ctorInfo) ctors)
    go (_, P.TypeSynonym) =
      uncurry ITypeSynonym <$>
        M.lookup (N.ProperName <$> qul) (P.typeSynonyms env)
    go _ =
      Nothing

    makeTypeInfo
      :: [(String, Maybe P.Kind)]
      -> [(P.DataDeclType, (String, InfoConstructor))]
      -> InfoTarget
    makeTypeInfo vars ctors =
      let
        -- If there are no constructors, it must be a data dec
        ddt = fromMaybe P.Data . fmap fst . listToMaybe $ ctors
      in
        IType (InfoType ddt (P.ProperName unQul) vars) (map snd ctors)

getPrintableFromConstructors :: GetPrintable
getPrintableFromConstructors env qul =
  M.lookup (P.ProperName <$> qul) (P.dataConstructors env) >>= go
  where
    go
      :: (P.DataDeclType, N.ProperName 'N.TypeName, P.Type, [N.Ident])
      -> Maybe InfoTarget
    go (ddt, tn, t, is) =
      uncurry IConstructor . second (InfoConstructor t)
      <$>
        tyInfo (N.disqualify qul) ddt (const tn <$> qul)

    tyInfo
      :: String
      -> P.DataDeclType
      -> P.Qualified (N.ProperName 'N.TypeName)
      -> Maybe (InfoType, [P.Type])
    tyInfo ctorName ddt qNm@(N.Qualified _ nm) =
      first (InfoType ddt nm) <$> do
        (_, info) <- M.lookup qNm (P.types env)
        typeVars  <- extractTypeVars info
        ctors     <- extractConstructors ctorName info
        return (typeVars, ctors)

    extractTypeVars
      :: P.TypeKind
      -> Maybe [(String, Maybe P.Kind)]
    extractTypeVars = fmap fst . extractDataType

    extractDataType
      :: P.TypeKind
      -> Maybe
        ( [(String, Maybe P.Kind)]
        , [(N.ProperName 'N.ConstructorName, [P.Type])]
        )
    extractDataType (P.DataType vars ctors) = Just (vars, ctors)
    extractDataType _                       = Nothing

    extractConstructors
      :: String
      -> P.TypeKind
      -> Maybe [P.Type]
    extractConstructors nm =
      (>>= getByName) . fmap snd . extractDataType
      where
        getByName = lookup (N.ProperName nm)

-- | Browse a module and displays its signature
handleBrowse
  :: (MonadReader PSCiConfig m, MonadState PSCiState m, MonadIO m)
  => P.ModuleName
  -> m ()
handleBrowse moduleName = do
  st <- get
  env <- asks psciEnvironment
  if isModInEnv moduleName st
    then liftIO . putStrLn $ printModuleSignatures moduleName env
    else case lookupUnQualifiedModName moduleName st of
      Just unQualifiedName ->
        if isModInEnv unQualifiedName st
          then liftIO . putStrLn $ printModuleSignatures unQualifiedName env
          else failNotInEnv moduleName
      Nothing ->
        failNotInEnv moduleName
  where
    isModInEnv modName =
        any ((== modName) . P.getModuleName . fst) . psciLoadedExterns
    failNotInEnv modName =
        liftIO $ putStrLn $ "Module '" ++ N.runModuleName modName ++ "' is not valid."
    lookupUnQualifiedModName quaModName st =
        (\(modName,_,_) -> modName) <$> find ( \(_, _, mayQuaName) -> mayQuaName == Just quaModName) (psciImportedModules st)
