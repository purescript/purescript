Module Documentation
====================

Module Prelude
--------------

Types
~~~~~

Type Classes
~~~~~~~~~~~~

::

    class Alternative f where
      empty :: forall a. f a
      (<|>) :: forall a. f a -> f a -> f a

    class Applicative f where
      pure :: forall a. a -> f a
      (<*>) :: forall a b. f (a -> b) -> f a -> f b

    class Bits b where
      (&) :: b -> b -> b
      (|) :: b -> b -> b
      (^) :: b -> b -> b
      shl :: b -> Prim.Number -> b
      shr :: b -> Prim.Number -> b
      zshr :: b -> Prim.Number -> b
      complement :: b -> b

    class BoolLike b where
      (&&) :: b -> b -> b
      (||) :: b -> b -> b
      not :: b -> b

    class Category a where
      id :: forall t. a t t
      (<<<) :: forall b c d. a c d -> a b c -> a b d
      (>>>) :: forall b c d. a b c -> a c d -> a b d

    class Eq a where
      (==) :: a -> a -> Prim.Boolean
      (/=) :: a -> a -> Prim.Boolean

    class Functor f where
      (<$>) :: forall a b. (a -> b) -> f a -> f b

    class Monad m where
      return :: forall a. a -> m a
      (>>=) :: forall a b. m a -> (a -> m b) -> m b

    class Num a where
      (+) :: a -> a -> a
      (-) :: a -> a -> a
      (*) :: a -> a -> a
      (/) :: a -> a -> a
      (%) :: a -> a -> a
      negate :: a -> a

    class Ord a where
      (<) :: a -> a -> Prim.Boolean
      (>) :: a -> a -> Prim.Boolean
      (<=) :: a -> a -> Prim.Boolean
      (>=) :: a -> a -> Prim.Boolean

    class Read a where
      read :: Prim.String -> a

    class Show a where
      show :: a -> Prim.String

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

::

    (Monad ((m))) => instance applicativeFromMonad :: Applicative (m)

    instance bitsNumber :: Bits (Prim.Number)

    instance boolLikeBoolean :: BoolLike (Prim.Boolean)

    instance categoryArr :: Category (Prim.Function)

    (Eq ((a))) => instance eqArray :: Eq ([a])

    instance eqBoolean :: Eq (Prim.Boolean)

    instance eqNumber :: Eq (Prim.Number)

    instance eqString :: Eq (Prim.String)

    (Applicative ((f))) => instance functorFromApplicative :: Functor (f)

    instance numNumber :: Num (Prim.Number)

    instance ordNumber :: Ord (Prim.Number)

    instance readBoolean :: Read (Prim.Boolean)

    instance readNumber :: Read (Prim.Number)

    instance readString :: Read (Prim.String)

    instance showBoolean :: Show (Prim.Boolean)

    instance showNumber :: Show (Prim.Number)

    instance showString :: Show (Prim.String)

Values
~~~~~~

::

    (!!) :: forall a. [a] -> Prim.Number -> a

    (#) :: forall a b. a -> (a -> b) -> b

    ($) :: forall a b. (a -> b) -> a -> b

    (++) :: Prim.String -> Prim.String -> Prim.String

    boolAnd :: Prim.Boolean -> Prim.Boolean -> Prim.Boolean

    boolNot :: Prim.Boolean -> Prim.Boolean

    boolOr :: Prim.Boolean -> Prim.Boolean -> Prim.Boolean

    const :: forall a b. a -> b -> a

    flip :: forall a b c. (a -> b -> c) -> b -> a -> c

    numAdd :: Prim.Number -> Prim.Number -> Prim.Number

    numAnd :: Prim.Number -> Prim.Number -> Prim.Number

    numComplement :: Prim.Number -> Prim.Number

    numDiv :: Prim.Number -> Prim.Number -> Prim.Number

    numGreater :: Prim.Number -> Prim.Number -> Prim.Boolean

    numGreaterEq :: Prim.Number -> Prim.Number -> Prim.Boolean

    numLess :: Prim.Number -> Prim.Number -> Prim.Boolean

    numLessEq :: Prim.Number -> Prim.Number -> Prim.Boolean

    numMod :: Prim.Number -> Prim.Number -> Prim.Number

    numMul :: Prim.Number -> Prim.Number -> Prim.Number

    numNegate :: Prim.Number -> Prim.Number

    numOr :: Prim.Number -> Prim.Number -> Prim.Number

    numShl :: Prim.Number -> Prim.Number -> Prim.Number

    numShr :: Prim.Number -> Prim.Number -> Prim.Number

    numSub :: Prim.Number -> Prim.Number -> Prim.Number

    numXor :: Prim.Number -> Prim.Number -> Prim.Number

    numZshr :: Prim.Number -> Prim.Number -> Prim.Number

    readNumberImpl :: Prim.String -> Prim.Number

    showNumberImpl :: Prim.Number -> Prim.String

    unsafeRefEq :: forall a. a -> a -> Prim.Boolean

    unsafeRefIneq :: forall a. a -> a -> Prim.Boolean

Module Data.Monoid
------------------

Types
~~~~~

Type Classes
~~~~~~~~~~~~

::

    class Monoid m where
      mempty :: m
      (<>) :: m -> m -> m

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

::

    instance monoidArray :: Monoid ([a])

    instance monoidString :: Monoid (Prim.String)

Values
~~~~~~

::

    mconcat :: forall m. (Monoid m) => [m] -> m

Module Control.Monad
--------------------

Types
~~~~~

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

Values
~~~~~~

::

    (<=<) :: forall m a b c. (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c

    (>=>) :: forall m a b c. (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c

    foldM :: forall m a b. (Monad m) => (a -> b -> m a) -> a -> [b] -> m a

    join :: forall m a. (Monad m) => m (m a) -> m a

    mapM :: forall m a b. (Monad m) => (a -> m b) -> [a] -> m [b]

    replicateM :: forall m a. (Monad m) => Prim.Number -> m a -> m [a]

    sequence :: forall m a. (Monad m) => [m a] -> m [a]

    when :: forall m. (Monad m) => Prim.Boolean -> m {  } -> m {  }

    zipWithM :: forall m a b c. (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]

Module Data.Maybe
-----------------

Types
~~~~~

::

    data Maybe a where
      Nothing :: Maybe a
      Just :: a -> Maybe a

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

::

    instance applicativeMaybe :: Applicative (Maybe)

    instance functorMaybe :: Functor (Maybe)

    instance monadMaybe :: Monad (Maybe)

    (Show ((a))) => instance showMaybe :: Show (Maybe a)

Values
~~~~~~

::

    fromMaybe :: forall a. a -> Maybe a -> a

    maybe :: forall a b. b -> (a -> b) -> Maybe a -> b

Module Data.Either
------------------

Types
~~~~~

::

    data Either a b where
      Left :: a -> Either a b
      Right :: b -> Either a b

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

::

    instance applicativeEither :: Applicative (Either e)

    instance functorEither :: Functor (Either a)

    instance monadEither :: Monad (Either e)

    (Show ((a)),Show ((b))) => instance showEither :: Show (Either a b)

Values
~~~~~~

::

    either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c

Module Data.Array
-----------------

Types
~~~~~

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

::

    instance alternativeArray :: Alternative (Prim.Array)

    instance functorArray :: Functor (Prim.Array)

    instance monadArray :: Monad (Prim.Array)

    (Show ((a))) => instance showArray :: Show ([a])

Values
~~~~~~

::

    (:) :: forall a. a -> [a] -> [a]

    all :: forall a. (a -> Prim.Boolean) -> [a] -> Prim.Boolean

    any :: forall a. (a -> Prim.Boolean) -> [a] -> Prim.Boolean

    concat :: forall a. [a] -> [a] -> [a]

    concatMap :: forall a b. [a] -> (a -> [b]) -> [b]

    deleteAt :: forall a. Prim.Number -> Prim.Number -> [a] -> [a]

    drop :: forall a. Prim.Number -> [a] -> [a]

    filter :: forall a. (a -> Prim.Boolean) -> [a] -> [a]

    find :: forall a. (a -> Prim.Boolean) -> [a] -> Maybe a

    foldl :: forall a b. (b -> a -> b) -> b -> [a] -> b

    foldr :: forall a b. (a -> b -> a) -> a -> [b] -> a

    head :: forall a. [a] -> Maybe a

    indexOf :: forall a. [a] -> a -> Prim.Number

    insertAt :: forall a. Prim.Number -> a -> [a] -> [a]

    isEmpty :: forall a. [a] -> Prim.Boolean

    joinS :: [Prim.String] -> Prim.String

    joinWith :: [Prim.String] -> Prim.String -> Prim.String

    lastIndexOf :: forall a. [a] -> a -> Prim.Number

    length :: forall a. [a] -> Prim.Number

    map :: forall a b. (a -> b) -> [a] -> [b]

    push :: forall a. [a] -> a -> [a]

    range :: Prim.Number -> Prim.Number -> [Prim.Number]

    reverse :: forall a. [a] -> [a]

    shift :: forall a. [a] -> [a]

    singleton :: forall a. a -> [a]

    slice :: forall a. Prim.Number -> Prim.Number -> [a] -> [a]

    sort :: forall a. [a] -> [a]

    tail :: forall a. [a] -> Maybe [a]

    take :: forall a. Prim.Number -> [a] -> [a]

    updateAt :: forall a. Prim.Number -> a -> [a] -> [a]

    zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]

Module Data.Eq
--------------

Types
~~~~~

::

    data Ref a where
      Ref :: a -> Ref a

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

::

    instance eqRef :: Eq (Ref a)

Values
~~~~~~

::

    liftRef :: forall a b. (a -> a -> b) -> Ref a -> Ref a -> b

    refEq :: forall a. Ref a -> Ref a -> Prim.Boolean

    refIneq :: forall a. Ref a -> Ref a -> Prim.Boolean

Module Data.Array.Unsafe
------------------------

Types
~~~~~

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

Values
~~~~~~

::

    head :: forall a. [a] -> a

    tail :: forall a. [a] -> [a]

Module Data.Tuple
-----------------

Types
~~~~~

::

    data Tuple a b where
      Tuple :: a -> b -> Tuple a b

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

::

    (Show ((a)),Show ((b))) => instance showTuple :: Show (Tuple a b)

Values
~~~~~~

::

    curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c

    uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c

    unzip :: forall a b. [Tuple a b] -> Tuple [a] [b]

    zip :: forall a b. [a] -> [b] -> [Tuple a b]

Module Data.String
------------------

Types
~~~~~

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

Values
~~~~~~

::

    charAt :: Prim.Number -> Prim.String -> Prim.String

    indexOfS :: Prim.String -> Prim.String -> Prim.Number

    lastIndexOfS :: Prim.String -> Prim.String -> Prim.Number

    lengthS :: Prim.String -> Prim.Number

    localeCompare :: Prim.String -> Prim.String -> Prim.Number

    replace :: Prim.String -> Prim.String -> Prim.String -> Prim.String

    sliceS :: Prim.Number -> Prim.Number -> Prim.String -> Prim.String

    split :: Prim.String -> Prim.String -> [Prim.String]

    substr :: Prim.Number -> Prim.Number -> Prim.String -> Prim.String

    substring :: Prim.Number -> Prim.Number -> Prim.String -> Prim.String

    toLower :: Prim.String -> Prim.String

    toUpper :: Prim.String -> Prim.String

    trim :: Prim.String -> Prim.String

Module Data.String.Regex
------------------------

Types
~~~~~

::

    data Regex :: *

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

Values
~~~~~~

::

    match :: Regex -> Prim.String -> [Prim.String]

    regex :: Prim.String -> Prim.String -> Regex

    replaceR :: Regex -> Prim.String -> Prim.String -> Prim.String

    search :: Regex -> Prim.String -> Prim.Number

    test :: Regex -> Prim.String -> Prim.Boolean

Module Global
-------------

Types
~~~~~

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

Values
~~~~~~

::

    decodeURI :: Prim.String -> Prim.String

    decodeURIComponent :: Prim.String -> Prim.String

    encodeURI :: Prim.String -> Prim.String

    encodeURIComponent :: Prim.String -> Prim.String

    infinity :: Prim.Number

    isFinite :: Prim.Number -> Prim.Boolean

    isNaN :: Prim.Number -> Prim.Boolean

    nan :: Prim.Number

    parseFloat :: Prim.String -> Prim.Number

    parseInt :: Prim.String -> Prim.Number

    toExponential :: Prim.Number -> Prim.String

    toFixed :: Prim.Number -> Prim.Number -> Prim.String

    toPrecision :: Prim.Number -> Prim.Number -> Prim.String

Module Math
-----------

Types
~~~~~

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

Values
~~~~~~

::

    abs :: Prim.Number -> Prim.Number

    aceil :: Prim.Number -> Prim.Number

    acos :: Prim.Number -> Prim.Number

    asin :: Prim.Number -> Prim.Number

    atan :: Prim.Number -> Prim.Number

    atan2 :: Prim.Number -> Prim.Number -> Prim.Number

    cos :: Prim.Number -> Prim.Number

    e :: Prim.Number

    exp :: Prim.Number -> Prim.Number

    floor :: Prim.Number -> Prim.Number

    ln10 :: Prim.Number

    ln2 :: Prim.Number

    log :: Prim.Number -> Prim.Number

    log10e :: Prim.Number

    log2e :: Prim.Number

    max :: Prim.Number -> Prim.Number -> Prim.Number

    min :: Prim.Number -> Prim.Number -> Prim.Number

    pi :: Prim.Number

    pow :: Prim.Number -> Prim.Number -> Prim.Number

    round :: Prim.Number -> Prim.Number

    sin :: Prim.Number -> Prim.Number

    sqrt :: Prim.Number -> Prim.Number

    sqrt1_2 :: Prim.Number

    sqrt2 :: Prim.Number

    tan :: Prim.Number -> Prim.Number

Module Control.Monad.Eff
------------------------

Types
~~~~~

::

    data Eff :: # ! -> * -> *

    type Pure a = forall e. Eff e a

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

::

    instance monadEff :: Monad (Eff e)

Values
~~~~~~

::

    bindEff :: forall e a b. Eff e a -> (a -> Eff e b) -> Eff e b

    forE :: forall e. Prim.Number -> Prim.Number -> (Prim.Number -> Eff e {  }) -> Eff e {  }

    foreachE :: forall e a. [a] -> (a -> Eff e {  }) -> Eff e {  }

    retEff :: forall e a. a -> Eff e a

    runPure :: forall a. Pure a -> a

    untilE :: forall e. Eff e Prim.Boolean -> Eff e {  }

    whileE :: forall e a. Eff e Prim.Boolean -> Eff e a -> Eff e {  }

Module Control.Monad.Eff.Unsafe
-------------------------------

Types
~~~~~

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

Values
~~~~~~

::

    unsafeInterleaveEff :: forall eff1 eff2 a. Eff eff1 a -> Eff eff2 a

Module Random
-------------

Types
~~~~~

::

    data Random :: !

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

Values
~~~~~~

::

    random :: forall e. Eff (random :: Random | e) Prim.Number

Module Control.Monad.Error
--------------------------

Types
~~~~~

::

    data Error :: * -> !

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

Values
~~~~~~

::

    catchError :: forall e r a. (e -> Eff r a) -> Eff (err :: Error e | r) a -> Eff r a

    throwError :: forall a e r. e -> Eff (err :: Error e | r) a

Module Data.IORef
-----------------

Types
~~~~~

::

    data IORef :: * -> *

    data Ref :: !

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

Values
~~~~~~

::

    modifyIORef :: forall s r. IORef s -> (s -> s) -> Eff (ref :: Ref | r) {  }

    newIORef :: forall s r. s -> Eff (ref :: Ref | r) (IORef s)

    readIORef :: forall s r. IORef s -> Eff (ref :: Ref | r) s

    unsafeRunIORef :: forall eff a. Eff (ref :: Ref | eff) a -> Eff eff a

    writeIORef :: forall s r. IORef s -> s -> Eff (ref :: Ref | r) {  }

Module Debug.Trace
------------------

Types
~~~~~

::

    data Trace :: !

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

Values
~~~~~~

::

    print :: forall a r. (Show a) => a -> Eff (trace :: Trace | r) {  }

    trace :: forall r. Prim.String -> Eff (trace :: Trace | r) {  }

Module Control.Monad.ST
-----------------------

Types
~~~~~

::

    data ST :: * -> !

    data STArray :: * -> * -> *

    data STRef :: * -> * -> *

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

Values
~~~~~~

::

    modifySTRef :: forall a h r. STRef h a -> (a -> a) -> Eff (st :: ST h | r) a

    newSTArray :: forall a h r. Prim.Number -> a -> Eff (st :: ST h | r) (STArray h a)

    newSTRef :: forall a h r. a -> Eff (st :: ST h | r) (STRef h a)

    peekSTArray :: forall a h r. STArray h a -> Eff (st :: ST h | r) a

    pokeSTArray :: forall a h r. STArray h a -> Prim.Number -> a -> Eff (st :: ST h | r) a

    readSTRef :: forall a h r. STRef h a -> Eff (st :: ST h | r) a

    runST :: forall a r. forall h. Eff (st :: ST h | r) a -> Eff r a

    runSTArray :: forall a r. forall h. Eff (st :: ST h | r) (STArray h a) -> Eff r [a]

    writeSTRef :: forall a h r. STRef h a -> a -> Eff (st :: ST h | r) a

Module Data.Enum
----------------

Types
~~~~~

Type Classes
~~~~~~~~~~~~

::

    class Enum a where
      toEnum :: Prim.Number -> Maybe a
      fromEnum :: a -> Prim.Number

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

Values
~~~~~~

