# Module Documentation
## Module Prelude

### Types

    data Ref a where
      Ref :: a -> Ref a


### Type Classes

    class Alternative f where
      empty :: forall a. f a
      (<|>) :: forall a. f a -> f a -> f a

    class Applicative f where
      pure :: forall a. a -> f a
      (<*>) :: forall b. forall a. f (a -> b) -> f a -> f b

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
      (<<<) :: forall d. forall c. forall b. a c d -> a b c -> a b d
      (>>>) :: forall d. forall c. forall b. a b c -> a c d -> a b d

    class Eq a where
      (==) :: a -> a -> Prim.Boolean
      (/=) :: a -> a -> Prim.Boolean

    class Functor f where
      (<$>) :: forall b. forall a. (a -> b) -> f a -> f b

    class Monad m where
      return :: forall a. a -> m a
      (>>=) :: forall b. forall a. m a -> (a -> m b) -> m b

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


### Type Class Instances

    (Monad (m)) => instance Applicative m

    instance Bits Prim.Number

    instance BoolLike Prim.Boolean

    instance Category Prim.Function

    instance Eq Ref a

    instance Eq Prim.String

    instance Eq Prim.Number

    instance Eq Prim.Boolean

    (Eq (a)) => instance Eq [a]

    (Applicative (f)) => instance Functor f

    instance Num Prim.Number

    instance Ord Prim.Number

    instance Prelude.Show Prim.Number

    instance Read Prim.String

    instance Read Prim.Boolean

    instance Read Prim.Number

    instance Show Prim.String

    instance Show Prim.Boolean


### Values

    (!!) :: forall a. [a] -> Prim.Number -> a

    (#) :: forall b. forall a. a -> (a -> b) -> b

    ($) :: forall b. forall a. (a -> b) -> a -> b

    (++) :: Prim.String -> Prim.String -> Prim.String

    boolAnd :: Prim.Boolean -> Prim.Boolean -> Prim.Boolean

    boolNot :: Prim.Boolean -> Prim.Boolean

    boolOr :: Prim.Boolean -> Prim.Boolean -> Prim.Boolean

    const :: forall b. forall a. a -> b -> a

    flip :: forall c. forall b. forall a. (a -> b -> c) -> b -> a -> c

    liftRef :: forall b. forall a. (a -> a -> b) -> Ref a -> Ref a -> b

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

    readNumber :: Prim.String -> Prim.Number

    refEq :: forall a. Ref a -> Ref a -> Prim.Boolean

    refIneq :: forall a. Ref a -> Ref a -> Prim.Boolean

    showNumber :: Prim.Number -> Prim.String

    unsafeRefEq :: forall a. a -> a -> Prim.Boolean

    unsafeRefIneq :: forall a. a -> a -> Prim.Boolean


## Module Data.Monoid

### Types


### Type Classes

    class Monoid m where
      mempty :: m
      (<>) :: m -> m -> m


### Type Class Instances

    instance Monoid Prim.String

    instance Monoid [a]


### Values

    mconcat :: forall m. (Monoid (m)) => [m] -> m


## Module Control.Monad

### Types


### Type Classes


### Type Class Instances


### Values

    (<=<) :: forall c. forall b. forall a. forall m. (Monad (m)) => (b -> m c) -> (a -> m b) -> a -> m c

    (>=>) :: forall c. forall b. forall a. forall m. (Monad (m)) => (a -> m b) -> (b -> m c) -> a -> m c

    foldM :: forall b. forall a. forall m. (Monad (m)) => (a -> b -> m a) -> a -> [b] -> m a

    join :: forall a. forall m. (Monad (m)) => m (m a) -> m a

    mapM :: forall b. forall a. forall m. (Monad (m)) => (a -> m b) -> [a] -> m [b]

    replicateM :: forall a. forall m. (Monad (m)) => Prim.Number -> m a -> m [a]

    sequence :: forall a. forall m. (Monad (m)) => [m a] -> m [a]

    when :: forall m. (Monad (m)) => Prim.Boolean -> m {  } -> m {  }


## Module Data.Maybe

### Types

    data Maybe a where
      Nothing :: Maybe a
      Just :: a -> Maybe a


### Type Classes


### Type Class Instances

    instance Prelude.Applicative Maybe

    instance Prelude.Functor Maybe

    instance Prelude.Monad Maybe

    (Show (a)) => instance Prelude.Show Maybe a


### Values

    fromMaybe :: forall a. a -> Maybe a -> a

    maybe :: forall b. forall a. b -> (a -> b) -> Maybe a -> b


## Module Data.Either

### Types

    data Either a b where
      Left :: a -> Either a b
      Right :: b -> Either a b


### Type Classes


### Type Class Instances

    instance Prelude.Applicative Either e

    instance Prelude.Functor Either a

    instance Prelude.Monad Either e

    (Show (a),Show (b)) => instance Prelude.Show Either a b


### Values

    either :: forall c. forall b. forall a. (a -> c) -> (b -> c) -> Either a b -> c


## Module Data.Array

### Types


### Type Classes


### Type Class Instances

    instance Prelude.Alternative Prim.Array

    instance Prelude.Functor Prim.Array

    instance Prelude.Monad Prim.Array

    (Prelude.Show (a)) => instance Prelude.Show [a]


### Values

    (:) :: forall a. a -> [a] -> [a]

    all :: forall a. (a -> Prim.Boolean) -> [a] -> Prim.Boolean

    any :: forall a. (a -> Prim.Boolean) -> [a] -> Prim.Boolean

    concat :: forall a. [a] -> [a] -> [a]

    concatMap :: forall b. forall a. [a] -> (a -> [b]) -> [b]

    filter :: forall a. (a -> Prim.Boolean) -> [a] -> [a]

    find :: forall a. (a -> Prim.Boolean) -> [a] -> Maybe a

    foldl :: forall b. forall a. (a -> b -> b) -> b -> [a] -> b

    foldr :: forall b. forall a. (a -> b -> a) -> a -> [b] -> a

    head :: forall a. [a] -> Maybe a

    indexOf :: forall a. [a] -> a -> Prim.Number

    isEmpty :: forall a. [a] -> Prim.Boolean

    joinS :: [Prim.String] -> Prim.String

    joinWith :: [Prim.String] -> Prim.String -> Prim.String

    lastIndexOf :: forall a. [a] -> a -> Prim.Number

    length :: forall a. [a] -> Prim.Number

    map :: forall b. forall a. (a -> b) -> [a] -> [b]

    push :: forall a. [a] -> a -> [a]

    range :: Prim.Number -> Prim.Number -> [Prim.Number]

    reverse :: forall a. [a] -> [a]

    shift :: forall a. [a] -> [a]

    singleton :: forall a. a -> [a]

    slice :: forall a. Prim.Number -> Prim.Number -> [a] -> [a]

    sort :: forall a. [a] -> [a]

    splice :: forall a. Prim.Number -> Prim.Number -> [a] -> [a] -> [a]

    tail :: forall a. [a] -> Maybe [a]

    zipWith :: forall c. forall b. forall a. (a -> b -> c) -> [a] -> [b] -> [c]


## Module Data.Array.Unsafe

### Types


### Type Classes


### Type Class Instances


### Values

    head :: forall a. [a] -> a

    tail :: forall a. [a] -> [a]


## Module Data.Tuple

### Types

    data Tuple a b where
      Tuple :: a -> b -> Tuple a b


### Type Classes


### Type Class Instances

    (Prelude.Show (a),Prelude.Show (b)) => instance Prelude.Show Tuple a b


### Values

    curry :: forall c. forall b. forall a. (Tuple a b -> c) -> a -> b -> c

    uncurry :: forall c. forall b. forall a. (a -> b -> c) -> Tuple a b -> c

    unzip :: forall b. forall a. [Tuple a b] -> Tuple [a] [b]

    zip :: forall b. forall a. [a] -> [b] -> [Tuple a b]


## Module Data.String

### Types


### Type Classes


### Type Class Instances


### Values

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


## Module Data.String.Regex

### Types

    data Regex :: *


### Type Classes


### Type Class Instances


### Values

    match :: Regex -> Prim.String -> [Prim.String]

    regex :: Prim.String -> Prim.String -> Regex

    replaceR :: Regex -> Prim.String -> Prim.String -> Prim.String

    search :: Regex -> Prim.String -> Prim.Number

    test :: Regex -> Prim.String -> Prim.Boolean


## Module Global

### Types


### Type Classes


### Type Class Instances


### Values

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


## Module Math

### Types


### Type Classes


### Type Class Instances


### Values

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

    max :: Prim.Number -> Prim.Number

    min :: Prim.Number -> Prim.Number

    pi :: Prim.Number

    pow :: Prim.Number -> Prim.Number

    round :: Prim.Number -> Prim.Number

    sin :: Prim.Number -> Prim.Number

    sqrt :: Prim.Number -> Prim.Number

    sqrt1_2 :: Prim.Number

    sqrt2 :: Prim.Number

    tan :: Prim.Number -> Prim.Number


## Module Control.Monad.Eff

### Types

    data Eff :: # ! -> * -> *

    type Pure a = forall e. Eff e a


### Type Classes


### Type Class Instances

    instance Prelude.Monad Eff e


### Values

    bindEff :: forall b. forall a. forall e. Eff e a -> (a -> Eff e b) -> Eff e b

    forE :: forall e. Prim.Number -> Prim.Number -> (Prim.Number -> Eff e {  }) -> Eff e {  }

    foreachE :: forall a. forall e. [a] -> (a -> Eff e {  }) -> Eff e {  }

    retEff :: forall a. forall e. a -> Eff e a

    runPure :: forall a. Pure a -> a

    untilE :: forall e. Eff e Prim.Boolean -> Eff e {  }

    whileE :: forall a. forall e. Eff e Prim.Boolean -> Eff e a -> Eff e {  }


## Module Random

### Types

    data Random :: !


### Type Classes


### Type Class Instances


### Values

    random :: forall e. Eff (random :: Random | e) Prim.Number


## Module Control.Monad.Error

### Types

    data Error :: * -> !


### Type Classes


### Type Class Instances


### Values

    catchError :: forall a. forall r. forall e. (e -> Eff r a) -> Eff (err :: Error e | r) a -> Eff r a

    throwError :: forall r. forall e. forall a. e -> Eff (err :: Error e | r) a


## Module Data.IORef

### Types

    data IORef :: * -> *

    data Ref :: !


### Type Classes


### Type Class Instances


### Values

    modifyIORef :: forall r. forall s. IORef s -> (s -> s) -> Eff (ref :: Ref | r) {  }

    newIORef :: forall r. forall s. s -> Eff (ref :: Ref | r) (IORef s)

    readIORef :: forall r. forall s. IORef s -> Eff (ref :: Ref | r) s

    unsafeRunIORef :: forall a. forall eff. Eff (ref :: Ref | eff) a -> Eff eff a

    writeIORef :: forall r. forall s. IORef s -> s -> Eff (ref :: Ref | r) {  }


## Module Debug.Trace

### Types

    data Trace :: !


### Type Classes


### Type Class Instances


### Values

    print :: forall r. forall a. (Prelude.Show (a)) => a -> Eff (trace :: Trace | r) {  }

    trace :: forall r. Prim.String -> Eff (trace :: Trace | r) {  }


## Module Control.Monad.ST

### Types

    data ST :: * -> !

    data STArray :: * -> * -> *

    data STRef :: * -> * -> *


### Type Classes


### Type Class Instances


### Values

    modifySTRef :: forall r. forall h. forall a. STRef h a -> (a -> a) -> Eff (st :: ST h | r) a

    newSTArray :: forall r. forall h. forall a. Prim.Number -> a -> Eff (st :: ST h | r) (STArray h a)

    newSTRef :: forall r. forall h. forall a. a -> Eff (st :: ST h | r) (STRef h a)

    peekSTArray :: forall r. forall h. forall a. STArray h a -> Eff (st :: ST h | r) a

    pokeSTArray :: forall r. forall h. forall a. STArray h a -> Prim.Number -> a -> Eff (st :: ST h | r) a

    readSTRef :: forall r. forall h. forall a. STRef h a -> Eff (st :: ST h | r) a

    runST :: forall r. forall a. forall h. Eff (st :: ST h | r) a -> Eff r a

    runSTArray :: forall r. forall a. forall h. Eff (st :: ST h | r) (STArray h a) -> Eff r [a]

    writeSTRef :: forall r. forall h. forall a. STRef h a -> a -> Eff (st :: ST h | r) a


## Module Data.Enum

### Types


### Type Classes

    class Enum a where
      toEnum :: Prim.Number -> Maybe a
      fromEnum :: a -> Prim.Number


### Type Class Instances


### Values



