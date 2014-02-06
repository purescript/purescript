Module Documentation
====================

Module Prelude
--------------

Types
~~~~~

::

    data Array :: * -> *

    data Boolean :: *

    data Function :: * -> * -> *

    data Number :: *

    data Ref a where
      Ref :: a -> Ref a

    data String :: *

Type Classes
~~~~~~~~~~~~

::

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
      shl :: b -> Prelude.Number -> b
      shr :: b -> Prelude.Number -> b
      zshr :: b -> Prelude.Number -> b
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
      (==) :: a -> a -> Prelude.Boolean
      (/=) :: a -> a -> Prelude.Boolean

    class Functor f where
      (<$>) :: forall b. forall a. (a -> b) -> f a -> f b

    class Monad m where
      $return :: forall a. a -> m a
      (>>=) :: forall b. forall a. m a -> (a -> m b) -> m b

    class Num a where
      (+) :: a -> a -> a
      (-) :: a -> a -> a
      (*) :: a -> a -> a
      (/) :: a -> a -> a
      (%) :: a -> a -> a
      negate :: a -> a

    class Ord a where
      (<) :: a -> a -> Prelude.Boolean
      (>) :: a -> a -> Prelude.Boolean
      (<=) :: a -> a -> Prelude.Boolean
      (>=) :: a -> a -> Prelude.Boolean

    class Read a where
      read :: Prelude.String -> a

    class Show a where
      show :: a -> Prelude.String

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

::

    (Monad (m)) => instance Applicative m

    instance Bits Prelude.Number

    instance BoolLike Prelude.Boolean

    instance Category Prelude.Function

    instance Eq Ref a

    instance Eq Prelude.String

    instance Eq Prelude.Number

    instance Eq Prelude.Boolean

    (Eq (a)) => instance Eq [a]

    (Applicative (f)) => instance Functor f

    instance Num Prelude.Number

    instance Ord Prelude.Number

    instance Prelude.Show Prelude.Number

    instance Read Prelude.String

    instance Read Prelude.Boolean

    instance Show Prelude.String

    instance Show Prelude.Boolean

Values
~~~~~~

::

    $const :: forall b. forall a. a -> b -> a

    (!!) :: forall a. [a] -> Prelude.Number -> a

    (#) :: forall b. forall a. a -> (a -> b) -> b

    ($) :: forall b. forall a. (a -> b) -> a -> b

    (++) :: Prelude.String -> Prelude.String -> Prelude.String

    boolAnd :: Prelude.Boolean -> Prelude.Boolean -> Prelude.Boolean

    boolNot :: Prelude.Boolean -> Prelude.Boolean

    boolOr :: Prelude.Boolean -> Prelude.Boolean -> Prelude.Boolean

    flip :: forall c. forall b. forall a. (a -> b -> c) -> b -> a -> c

    numAdd :: Prelude.Number -> Prelude.Number -> Prelude.Number

    numAnd :: Prelude.Number -> Prelude.Number -> Prelude.Number

    numComplement :: Prelude.Number -> Prelude.Number

    numDiv :: Prelude.Number -> Prelude.Number -> Prelude.Number

    numGreater :: Prelude.Number -> Prelude.Number -> Prelude.Boolean

    numGreaterEq :: Prelude.Number -> Prelude.Number -> Prelude.Boolean

    numLess :: Prelude.Number -> Prelude.Number -> Prelude.Boolean

    numLessEq :: Prelude.Number -> Prelude.Number -> Prelude.Boolean

    numMod :: Prelude.Number -> Prelude.Number -> Prelude.Number

    numMul :: Prelude.Number -> Prelude.Number -> Prelude.Number

    numNegate :: Prelude.Number -> Prelude.Number

    numOr :: Prelude.Number -> Prelude.Number -> Prelude.Number

    numShl :: Prelude.Number -> Prelude.Number -> Prelude.Number

    numShr :: Prelude.Number -> Prelude.Number -> Prelude.Number

    numSub :: Prelude.Number -> Prelude.Number -> Prelude.Number

    numXor :: Prelude.Number -> Prelude.Number -> Prelude.Number

    numZshr :: Prelude.Number -> Prelude.Number -> Prelude.Number

    refEq :: forall a. Ref a -> Ref a -> Prelude.Boolean

    refIneq :: forall a. Ref a -> Ref a -> Prelude.Boolean

    showNumber :: Prelude.Number -> Prelude.String

    unsafeRefEq :: forall a. a -> a -> Prelude.Boolean

    unsafeRefIneq :: forall a. a -> a -> Prelude.Boolean

Module Monoid
-------------

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

    instance Monoid Prelude.String

Values
~~~~~~

::

    mconcat :: forall m. (Monoid (m)) => [m] -> m

Module Monad
------------

Types
~~~~~

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

Values
~~~~~~

::

    (<=<) :: forall c. forall b. forall a. forall m. (Monad (m)) => (b -> m c) -> (a -> m b) -> a -> m c

    (>=>) :: forall c. forall b. forall a. forall m. (Monad (m)) => (a -> m b) -> (b -> m c) -> a -> m c

    foldM :: forall b. forall a. forall m. (Monad (m)) => (a -> b -> m a) -> a -> [b] -> m a

    join :: forall a. forall m. (Monad (m)) => m (m a) -> m a

    mapM :: forall b. forall a. forall m. (Monad (m)) => (a -> m b) -> [a] -> m [b]

    replicateM :: forall a. forall m. (Monad (m)) => Prelude.Number -> m a -> m [a]

    sequence :: forall a. forall m. (Monad (m)) => [m a] -> m [a]

    when :: forall m. (Monad (m)) => Prelude.Boolean -> m {  } -> m {  }

Module Maybe
------------

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

    instance Prelude.Monad Maybe

Values
~~~~~~

::

    fromMaybe :: forall a. a -> Maybe a -> a

    maybe :: forall b. forall a. b -> (a -> b) -> Maybe a -> b

Module Either
-------------

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

    instance Prelude.Monad Either e

Values
~~~~~~

::

    either :: forall c. forall b. forall a. (a -> c) -> (b -> c) -> Either a b -> c

Module Arrays
-------------

Types
~~~~~

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

::

    instance Prelude.Alternative Prelude.Array

    instance Prelude.Monad Prelude.Array

    (Prelude.Show (a)) => instance Prelude.Show [a]

Values
~~~~~~

::

    (:) :: forall a. a -> [a] -> [a]

    all :: forall a. (a -> Prelude.Boolean) -> [a] -> Prelude.Boolean

    any :: forall a. (a -> Prelude.Boolean) -> [a] -> Prelude.Boolean

    concat :: forall a. [a] -> [a] -> [a]

    concatMap :: forall b. forall a. [a] -> (a -> [b]) -> [b]

    filter :: forall a. (a -> Prelude.Boolean) -> [a] -> [a]

    foldl :: forall b. forall a. (a -> b -> b) -> b -> [a] -> b

    foldr :: forall b. forall a. (a -> b -> a) -> a -> [b] -> a

    head :: forall a. [a] -> a

    headSafe :: forall a. [a] -> Maybe a

    indexOf :: forall a. [a] -> a -> Prelude.Number

    isEmpty :: forall a. [a] -> Prelude.Boolean

    joinS :: [Prelude.String] -> Prelude.String

    joinWith :: [Prelude.String] -> Prelude.String -> Prelude.String

    lastIndexOf :: forall a. [a] -> a -> Prelude.Number

    length :: forall a. [a] -> Prelude.Number

    map :: forall b. forall a. (a -> b) -> [a] -> [b]

    push :: forall a. [a] -> a -> [a]

    range :: Prelude.Number -> Prelude.Number -> [Prelude.Number]

    reverse :: forall a. [a] -> [a]

    shift :: forall a. [a] -> [a]

    singleton :: forall a. a -> [a]

    slice :: forall a. Prelude.Number -> Prelude.Number -> [a] -> [a]

    sort :: forall a. [a] -> [a]

    splice :: forall a. Prelude.Number -> Prelude.Number -> [a] -> [a] -> [a]

    tail :: forall a. [a] -> [a]

    tailSafe :: forall a. [a] -> Maybe [a]

    zipWith :: forall c. forall b. forall a. (a -> b -> c) -> [a] -> [b] -> [c]

Module Tuple
------------

Types
~~~~~

::

    type Tuple a b = { snd :: b, fst :: a }

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

Values
~~~~~~

::

    curry :: forall c. forall b. forall a. (Tuple a b -> c) -> a -> b -> c

    tuple :: forall b. forall a. a -> b -> Tuple a b

    uncurry :: forall c. forall b. forall a. (a -> b -> c) -> Tuple a b -> c

    unzip :: forall b. forall a. [Tuple a b] -> Tuple [a] [b]

    zip :: forall b. forall a. [a] -> [b] -> [Tuple a b]

Module String
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

    charAt :: Prelude.Number -> Prelude.String -> Prelude.String

    indexOfS :: Prelude.String -> Prelude.String -> Prelude.Number

    lastIndexOfS :: Prelude.String -> Prelude.String -> Prelude.Number

    lengthS :: Prelude.String -> Prelude.Number

    localeCompare :: Prelude.String -> Prelude.String -> Prelude.Number

    replace :: Prelude.String -> Prelude.String -> Prelude.String -> Prelude.String

    sliceS :: Prelude.Number -> Prelude.Number -> Prelude.String -> Prelude.String

    split :: Prelude.String -> Prelude.String -> [Prelude.String]

    substr :: Prelude.Number -> Prelude.Number -> Prelude.String -> Prelude.String

    substring :: Prelude.Number -> Prelude.Number -> Prelude.String -> Prelude.String

    toLower :: Prelude.String -> Prelude.String

    toUpper :: Prelude.String -> Prelude.String

    trim :: Prelude.String -> Prelude.String

Module Regex
------------

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

    match :: Regex -> Prelude.String -> [Prelude.String]

    regex :: Prelude.String -> Prelude.String -> Regex

    replaceR :: Regex -> Prelude.String -> Prelude.String -> Prelude.String

    search :: Regex -> Prelude.String -> Prelude.Number

    test :: Regex -> Prelude.String -> Prelude.Boolean

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

    decodeURI :: Prelude.String -> Prelude.String

    decodeURIComponent :: Prelude.String -> Prelude.String

    encodeURI :: Prelude.String -> Prelude.String

    encodeURIComponent :: Prelude.String -> Prelude.String

    infinity :: Prelude.Number

    isFinite :: Prelude.Number -> Prelude.Boolean

    isNaN :: Prelude.Number -> Prelude.Boolean

    nan :: Prelude.Number

    parseFloat :: Prelude.String -> Prelude.Number

    parseInt :: Prelude.String -> Prelude.Number

    toExponential :: Prelude.Number -> Prelude.String

    toFixed :: Prelude.Number -> Prelude.Number -> Prelude.String

    toPrecision :: Prelude.Number -> Prelude.Number -> Prelude.String

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

    abs :: Prelude.Number -> Prelude.Number

    aceil :: Prelude.Number -> Prelude.Number

    acos :: Prelude.Number -> Prelude.Number

    asin :: Prelude.Number -> Prelude.Number

    atan :: Prelude.Number -> Prelude.Number

    atan2 :: Prelude.Number -> Prelude.Number -> Prelude.Number

    cos :: Prelude.Number -> Prelude.Number

    exp :: Prelude.Number -> Prelude.Number

    floor :: Prelude.Number -> Prelude.Number

    log :: Prelude.Number -> Prelude.Number

    max :: Prelude.Number -> Prelude.Number

    min :: Prelude.Number -> Prelude.Number

    pow :: Prelude.Number -> Prelude.Number

    round :: Prelude.Number -> Prelude.Number

    sin :: Prelude.Number -> Prelude.Number

    sqrt :: Prelude.Number -> Prelude.Number

    tan :: Prelude.Number -> Prelude.Number

Module Eff
----------

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

    instance Prelude.Monad Eff e

Values
~~~~~~

::

    bindEff :: forall b. forall a. forall e. Eff e a -> (a -> Eff e b) -> Eff e b

    forE :: forall e. Prelude.Number -> Prelude.Number -> (Prelude.Number -> Eff e {  }) -> Eff e {  }

    foreachE :: forall a. forall e. [a] -> (a -> Eff e {  }) -> Eff e {  }

    retEff :: forall a. forall e. a -> Eff e a

    runPure :: forall a. Pure a -> a

    untilE :: forall e. Eff e Prelude.Boolean -> Eff e {  }

    whileE :: forall e. Eff e Prelude.Boolean -> Eff e {  } -> Eff e {  }

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

    random :: forall e. Eff random :: Random | e Prelude.Number

Module Errors
-------------

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

    catchError :: forall a. forall r. forall e. (e -> Eff r a) -> Eff err :: Error e | r a -> Eff r a

    throwError :: forall r. forall e. forall a. e -> Eff err :: Error e | r a

Module IORef
------------

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

    modifyIORef :: forall r. forall s. IORef s -> (s -> s) -> Eff ref :: Ref | r {  }

    newIORef :: forall r. forall s. s -> Eff ref :: Ref | r (IORef s)

    readIORef :: forall r. forall s. IORef s -> Eff ref :: Ref | r s

    writeIORef :: forall r. forall s. IORef s -> s -> Eff ref :: Ref | r {  }

Module Trace
------------

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

    print :: forall r. forall a. (Prelude.Show (a)) => a -> Eff trace :: Trace | r {  }

    trace :: forall r. Prelude.String -> Eff trace :: Trace | r {  }

Module ST
---------

Types
~~~~~

::

    data ST :: * -> !

    data STRef :: * -> * -> *

Type Classes
~~~~~~~~~~~~

Type Class Instances
~~~~~~~~~~~~~~~~~~~~

Values
~~~~~~

::

    modifySTRef :: forall r. forall h. forall a. STRef h a -> (a -> a) -> Eff st :: ST h | r {  }

    newSTRef :: forall r. forall h. forall a. a -> Eff st :: ST h | r (STRef h a)

    readSTRef :: forall r. forall h. forall a. STRef h a -> Eff st :: ST h | r a

    runST :: forall r. forall a. forall h. Eff st :: ST h | r a -> Eff r a

    writeSTRef :: forall r. forall h. forall a. STRef h a -> a -> Eff st :: ST h | r {  }

