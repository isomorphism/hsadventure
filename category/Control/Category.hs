{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Control.Category
-- Copyright   : (c) 2007 Twan van Laarhoven
-- License     : BSD-style
-- Maintainer  : twanvl@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- TODO: documentation
--
--
-- Here is a schematic overview of the type classes from this module:
--
-- @
--                     /Lifting haskell functions/
--              +---> 'InvArrow' ---> 'RefArrow' ---> 'Arrow'
--              |
--              |      /Tuples/            
--              +---> 'CategoryPair' ----+--> 'CategoryFanOut'      |
--              |                      |                        | 
--              |                      +--> 'CategorySelect'      | /dual operations/
--              |                                               | 
--              |      /Eithers/                                  | /products(pairs) on the left/
--              +---> 'CategoryChoice' --+--> 'CategoryFanIn'       | /sums(eithers) on the right/
--              |                      |                        |
--  'Category' ---+                      +--> 'CategoryInject'      |
--              |
--              |      /Monoid operations/
--              +---> 'CategoryZero' --> 'CategoryPlus'
--              |
--              |      /Fixed points/
--              +---> 'CategoryLoop'
--              +---> 'CategoryCoLoop'
--              |
--              +---> 'CategoryApply'
-- @
--
-----------------------------------------------------------------------------
module Control.Category
        ( -- * Classes
          -- ** Basic categories
          Category(..)
        , CategoryConst(..)
        , (<<<), (>>>)
          -- ** Categories that contain Haskell functions
        , InvArrow(..)
        , RefArrow(..)
        , Arrow(..)
          -- ** Categories operating on pairs
        , CategoryPair(..)
        , CategorySelect(..)
        , CategoryFanOut(..)
          -- ** Categories supporting choice
        , CategoryChoice(..)
        , CategoryInject(..)
        , CategoryFanIn(..)
          -- ** Monoid operations
        , CategoryZero(..)
        , CategoryPlus(..)
          -- ** Fixed points
        , CategoryLoop(..)
        , CategoryCoLoop(..)
          -- ** Argument application
        , CategoryApply(..)
          -- * Wrapper types
        , Kleisli(..)
        , Env(..)
        , Static(..)
        , Dual(..)
        ) where

import qualified Prelude as P
import Prelude             (Functor(..), Either(..), Eq, Ord, Show, either, ($))
import Control.Applicative (Applicative(..), (<$>))
import Control.Monad       (Monad(..), MonadPlus(..), liftM, liftM2)
import Control.Monad.Fix   (MonadFix(..))
import Test.QuickCheck     (Arbitrary(..), property)

import Control.Category.Internal.Comparable (Comparable(..))

------------------------------------------------------------------------------------
-- Fixities

infixr 9 .
infixr 5 ///
infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||
infixr 1 >>>
infixr 1 <<<

------------------------------------------------------------------------------------
-- Classes : Basic

-- | A category is a type constructor with two arguments that is in some way a generalization of a function.
--
--   Values of type @cat a b@ are called /morphisms/ from @a@ to @b@.
--   Morphisms can be composed with the @.@ operator, similair to function composition.
--   And there is an identity morphim @id@, similair to the identity function.
--   
--   Instances must satisfy the laws:
--
--     [/Identity element/]       @id . f == f == f . id@
--
--     [/Associativity of /@'.'@] @f . (g . h) == (f . g) . h@
class Category cat where
      -- | The identity morphism, takes an object to itself.
      --   This generalizes the identity function.
      id  :: cat a a
      -- | Composition of morphisms in this category
      (.) :: (cat b c) -> (cat a b) -> (cat a c)

class Category cat => CategoryConst cat where
      const :: a -> cat b a

------------------------------------------------------------------------------------
-- Classes : Arrows

-- | @'InvArrow'@s are categories that can contain all invertible functions.
--
--   Instances must satisfy the laws
--
--     [/Lifting of /@'id'@] @arrInv id id == 'id'@
--
--     [/Lifting of /@'.'@]  @arrInv f g . arrInv h i == arrInv (f . h) (i . g)@
--
--     [/Inverse/]           @arrInv f g . arrInv g f == id@,
--                           if @f@ and @g@ are each others inverse.
--
class Category cat => InvArrow cat where
      -- | Construct a morphism from a function and its inverse.
      --
      --   When using @arrInv f g@, @f@ and @g@ must be each others inverse.
      --   In other words, the following must hold:
      --
      --   > g (f x) == x
      --   > f (g x) == x
      arrInv :: (a -> b) -> (b -> a) -> (cat a b)

-- |
--
--   Minimal complete definition: either @arrRef@ or @arrRef'@.
--
--   Instances must satisfy the laws:
--
--   > arrRef (\a -> (f a, g)) == arrInv f g
--   > arrRef' f (const . g)   == arrInv f g
class InvArrow cat => RefArrow cat where
      -- | Construct a morphism from a functional reference.
      --   When using @arrRef f@ the following must hold:
      --
      --   > snd (f x) (fst (f x)) == x
      arrRef :: (a -> (b, b -> a)) -> (cat a b)
      -- | Construct a morphism from a get and set function.
      --   When using @arrRef g s@ the following must hold:
      --
      --     [/Setting to same value has no effect/] @s x (g x) == x@
      -- 
      --     [/Getting retrieves the set value/]     @g (s x y) == y@
      arrRef' :: (a -> b) -> (b -> a -> a) -> (cat a b)
      
      arrRef  r   = arrRef' (\s -> P.fst (r s)) (\a s -> P.snd (r s) a)
      arrRef' g s = arrRef (\x -> (g x, \y -> s y x))

-- | Laws
--
--   Instances must satisfy the laws:
--
--     [/Generalization of /@'RefArrow'@] @'arrRef'' g s == arr g@
--
--   > arr f . arr g == arr (f . g)
--   > arr
--
class (RefArrow cat, CategoryConst cat) => Arrow cat where
      arr :: (a -> b) -> (cat a b)

------------------------------------------------------------------------------------
-- Classes : Pairs

-- | Categories that can operate on pairs.
--
--   Minimal complete definition: 'swap' and one of 'first' or '***'
--
--   Instances must satisfy the laws:
--
--   > swap . swap == id
--   > first id == id
--   > first f . swap == swap . second f
--   > first (f . g) == first f . first g
--   > f *** g == second g . first f == first f . second g <<<<<< ORDER OF SIDE EFFECTS
--
--    TODO: some of the last law may follow from parametric polymorphism
class Category cat => CategoryPair cat where
      -- | Send the first component of the input through the argument morphism,
      --   and copy the rest unchanged to the output.
      first  :: cat a b -> cat (a,c) (b,c)
      -- | Send the second component of the input through the argument morphism,
      --   and copy the rest unchanged to the output.
      second :: cat a b -> cat (c,a) (c,b)
      -- | Split the input between the two argument morphisms and combine their output.
      (***)  :: cat a b -> cat c d -> cat (a,c) (b,d)
      -- | Swap the elements of a pair
      swap   :: cat (a,b) (b,a)
      
      first f = f *** id
      second f = swap . first f . swap
      f *** g = second g . first f

-- | Categories that allow selecting sup-parts of pairs.
--
--   Minimal complete definition: one of 'fst' or 'snd'.
--
--   Instances must satisfy the laws:
--
--   > fst = snd . swap
--   > snd = fst . swap -- TODO: FOLLOWS FROM swap.swap=id
--
--   This doesn't hold for FRef :(
class CategoryPair cat => CategorySelect cat where
      -- | Select the first element of a pair
      fst    :: cat (a,b) a
      -- | Select the second element of a pair
      snd    :: cat (a,b) b
      
      fst = snd . swap
      snd = fst . swap

-- | Categories that allow duplicating input
--
--   Instances must satisfy the laws:
class CategoryPair cat => CategoryFanOut cat where
     (&&&) :: cat a b -> cat a c -> cat a (b,c)
     dup   :: cat a (a,a)

     f &&& g = (f *** g) . dup
     dup = id &&& id

------------------------------------------------------------------------------------
-- Classes : Choice

-- | Categories that can operate on 'Either's.
--   This class is the dual to 'CategoryPair'.
--
--   Minimal complete definition: 'mirror' and one of 'left' or '+++'.
--
--   Instances must satisfy the laws:
--
--   > mirror . mirror == id
--   > left id == id
--   > left f . mirror == mirror . right f
--
class Category cat => CategoryChoice cat where
      -- | Mirror the choices in an 'Either', 'Left' becomes 'Right' and vice-versa.
      mirror :: cat (Either a b) (Either b a)
      -- | Apply a morphism only to the 'Left' input values.
      left   :: cat a b -> cat (Either a c) (Either b c)
      -- | Apply a morphism only to the 'Right' input values.
      right  :: cat a b -> cat (Either c a) (Either c b)
      -- | Split the input between the two argument morphisms, retagging and merging their outputs.
      (+++)  :: cat a b -> cat c d -> cat (Either a c) (Either b d)
      
      left f = f +++ id
      right f = mirror . left f . mirror
      f +++ g = right g . left f

-- | Categories that allow 
--
--   Instances must satisfy the laws:
--
--   > inr = mirror . inl
--   > inl = mirror . inr
class CategoryChoice cat => CategoryInject cat where
      -- | The generalization of 'Left'
      inl    :: cat a (Either a b)
      -- | The generalization of 'Right'
      inr    :: cat b (Either a b) 

-- | Categories that allow merging of output (fanin).
--
--   Minimal complete definition: one of '|||' or 'untag'.
class CategoryChoice cat => CategoryFanIn cat where
     -- | Fanin: Split the input between the two argument morphisms and merge their outputs.
     (|||) :: cat a c -> cat b c -> cat (Either a b) c
     
     untag :: cat (Either a a) a

     f ||| g = untag . (f +++ g)
     untag = id ||| id

------------------------------------------------------------------------------------
-- Classes : Loose ends

-- | Categories that support the notion of failure
class Category cat => CategoryZero cat where
     zeroCat :: cat a b

-- | Categories providing
--   
--   Instances must satisfy the laws:
--
--   > zeroCat /// f == f
--   > f /// zeroCat == f
class CategoryZero cat => CategoryPlus cat where
     -- | Monoidal operation on categories,
     --   'zeroCat' is the identity element from '///'.
     (///) :: cat a b -> cat a b -> cat a b 


-- | Some categories allow application of inputs to other inputs.
class CategoryPair cat => CategoryApply cat where
      app :: cat (cat a b, a) b


-- | Categories providing a fixed point operation
class CategoryPair cat => CategoryLoop cat where
      -- | The loop operator expresses computations in which an output value is fed back as input,
      --   even though the computation occurs only once.
      loop :: cat (a,c) (b,c) -> cat a b

class CategoryChoice cat => CategoryCoLoop cat where
      -- | The dual of 'loop'.
      --   The computation is repeated until it returns a 'Right' result.
      coloop :: cat (Either c a) (Either c b) -> cat a b 


------------------------------------------------------------------------------------
-- Derived functions

-- | (.) with the arguments reversed
(>>>) :: Category cat => (cat a b) -> (cat b c) -> (cat a c)
f >>> g  = g . f

-- | Another name for composition that makes the flow of information clearer.
(<<<) :: Category cat => (cat b c) -> (cat a b) -> (cat a c)
(<<<) = (.)

------------------------------------------------------------------------------------
-- Instances : Functions

instance Category (->) where
      id x = x
      (f . g) x = f (g x)

instance CategoryConst (->) where
      const = P.const

instance CategoryPair (->) where
      first  f = f *** id
      second f = id *** f
      swap ~(x,y) = (y,x)
      (***) f g ~(x,y) = (f x, g y)

instance CategorySelect (->) where
      fst = P.fst
      snd = P.snd

instance CategoryFanOut (->) where
      (&&&) f g x = (f x, g x)
      dup x = (x,x)

instance CategoryChoice (->) where
      left  f = f +++ id
      right f = id +++ f
      mirror (Left  x) = Right x
      mirror (Right x) = Left  x
      f +++ g = (Left . f) ||| (Right . g)

instance CategoryInject (->) where
      inl = Left
      inr = Right

instance CategoryFanIn (->) where
      (|||) = either
      untag (Left  x) = x
      untag (Right x) = x

instance CategoryApply (->) where
      app (f,x) = f x

instance CategoryLoop (->) where
      loop f b = let (c,d) = f (b,d) in c

instance CategoryCoLoop (->) where
      coloop f a = lp (f (Right a))
        where lp (Left  c) = lp (f (Left c))
              lp (Right b) = b

instance InvArrow (->) where
      arrInv f _ = f

instance RefArrow (->) where
      arrRef f x = P.fst (f x)

instance Arrow (->) where
      arr f = f


-----------------------------------------------------------------------------
-- Type : Kleisli

-- | Kleisli 'Category' of a 'Monad'.
--
--   This are functions with a monadic result.
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Category (Kleisli m) where
      id = Kleisli return
      Kleisli f . Kleisli g = Kleisli (\b -> g b >>= f)

instance Monad m => CategoryConst (Kleisli m) where
      const a = Kleisli (\_ -> return a)

instance Monad m => InvArrow (Kleisli m) where
      arrInv f _ = arr f

instance Monad m => RefArrow (Kleisli m) where
      arrRef f = arr (P.fst . f)

instance Monad m => Arrow (Kleisli m) where
      arr f = Kleisli (return . f)

instance Monad m => CategoryPair (Kleisli m) where
      first  (Kleisli f) = Kleisli (\ ~(a,c) -> f a >>= \b -> return (b,c))
      second (Kleisli f) = Kleisli (\ ~(c,a) -> f a >>= \b -> return (c,b))
      swap = arr swap
      Kleisli f *** Kleisli g = Kleisli (\ ~(a,b) -> liftM2 (,) (f a) (g b) )

instance Monad m => CategorySelect (Kleisli m) where
      fst = arr P.fst
      snd = arr P.snd

instance Monad m => CategoryFanOut (Kleisli m) where
      dup = arr dup

instance Monad m => CategoryChoice (Kleisli m) where
      left  f = f +++ id
      right f = id +++ f
      f +++ g = (arr Left . f) ||| (arr Right . g)
      mirror  = arr mirror

instance Monad m => CategoryInject (Kleisli m) where
      inl = arr Left
      inr = arr Right

instance Monad m => CategoryFanIn (Kleisli m) where
      Kleisli f ||| Kleisli g = Kleisli (P.either f g)

instance MonadPlus m => CategoryZero (Kleisli m) where
      zeroCat = Kleisli (\_ -> mzero)
instance MonadPlus m => CategoryPlus (Kleisli m) where
      Kleisli f /// Kleisli g = Kleisli (\x -> f x `mplus` g x)

instance Monad m => CategoryApply (Kleisli m) where
      app = Kleisli (\(Kleisli f, x) -> f x)

instance MonadFix m => CategoryLoop (Kleisli m) where
      loop (Kleisli f) = Kleisli (liftM P.fst . mfix . f')
        where f' x y = f (x, snd y)

instance Monad m => CategoryCoLoop (Kleisli m) where
      coloop (Kleisli f) = Kleisli $ \a -> f (Right a) >>= lp
        where lp (Left  c) = f (Left c) >>= lp
              lp (Right b) = return b


-----------------------------------------------------------------------------
-- Type : Static arrow

-- | Apply an 'Applicative' functor to a category
newtype Static f cat a b = Static { runStatic :: f (cat a b) }
      deriving (Eq, Ord, Show)

instance (Applicative f, Category cat) => Category (Static f cat) where
      id = Static (pure id)
      Static f . Static g = Static $ (.) <$> f <*> g
instance (Applicative f, CategoryConst cat) => CategoryConst (Static f cat) where
      const x = Static $ pure (const x)

instance (Applicative f, InvArrow cat) => InvArrow (Static f cat) where
      arrInv f g = Static (pure (arrInv f g))
instance (Applicative f, RefArrow cat) => RefArrow (Static f cat) where
      arrRef f = Static (pure (arrRef f))
instance (Applicative f, Arrow cat) => Arrow (Static f cat) where
      arr f = Static (pure (arr f))

instance (Applicative f, CategoryPair cat) => CategoryPair (Static f cat) where
      swap = Static (pure swap)
      first  (Static f) = Static $ first  <$> f
      second (Static f) = Static $ second <$> f
      Static f *** Static g = Static $ (***) <$> f <*> g
instance (Applicative f, CategoryFanOut cat) => CategoryFanOut (Static f cat) where
      Static f &&& Static g = Static $ (&&&) <$> f <*> g
      dup = Static $ pure dup
instance (Applicative f, CategorySelect cat) => CategorySelect (Static f cat) where
      fst = Static $ pure fst
      snd = Static $ pure snd

instance (Applicative f, CategoryChoice cat) => CategoryChoice (Static f cat) where
      mirror = Static (pure mirror)
      left  (Static f) = Static $ left  <$> f
      right (Static f) = Static $ right <$> f
      Static f +++ Static g = Static $ (+++) <$> f <*> g
instance (Applicative f, CategoryFanIn cat) => CategoryFanIn (Static f cat) where
      Static f ||| Static g = Static $ (|||) <$> f <*> g
      untag = Static $ pure untag
instance (Applicative f, CategoryInject cat) => CategoryInject (Static f cat) where
      inl = Static $ pure inl
      inr = Static $ pure inr

-----------------------------------------------------------------------------
-- Type : Environment functor

-- | Fixing the first argument of an 'Arrow' gives rise to an 'Applicative' functor.
--
--   If the type is also in 'ArrowApply', hten

-- | The 'ArrowApply' class is equivalent to 'Monad': any monad gives rise
--   to a 'Kleisli' arrow, and any instance of 'ArrowApply' defines a monad.
newtype Env cat e a = Env { runEnv :: cat e a }
      deriving (Eq, Ord, Show)

instance Arrow cat => Functor (Env cat e) where
      fmap f (Env r) = Env (arr f . r)

instance (Arrow cat, CategoryFanOut cat) => Applicative (Env cat e) where
      pure x = Env (const x)
      Env f <*> Env g = Env (arr app . (f &&& g))

instance (Arrow cat, CategoryApply cat, CategoryFanOut cat) => Monad (Env cat e) where
      return x = Env (const x)
      Env m >>= f = Env $ app . (arr (runEnv . f) . m &&& id)

-----------------------------------------------------------------------------
-- Type : Dual category

-- | @Dual cat@ is the dual category to @cat@.
--   This means that the direction of all the morphisms is reversed.
newtype Dual cat a b = Dual { runDual :: cat b a }
      deriving (Eq, Ord, Show)

instance Category cat => Category (Dual cat) where
      id = Dual id
      Dual f . Dual g = Dual (g . f)

instance CategoryPair cat => CategoryPair (Dual cat) where
      first  (Dual f) = Dual (first  f)
      second (Dual f) = Dual (second f)
      Dual f *** Dual g = Dual (f *** g)
      swap = Dual swap

instance CategoryChoice cat => CategoryChoice (Dual cat) where
      left  (Dual f) = Dual (left  f)
      right (Dual f) = Dual (right f)
      Dual f +++ Dual g = Dual (f +++ g)
      mirror = Dual mirror

instance CategoryZero cat => CategoryZero (Dual cat) where
      zeroCat = Dual zeroCat

instance CategoryPlus cat => CategoryPlus (Dual cat) where
      Dual a /// Dual b = Dual (a /// b)

instance CategoryLoop cat => CategoryLoop (Dual cat) where
      loop (Dual f) = Dual (loop f)

instance InvArrow cat => InvArrow (Dual cat) where
      arrInv f g = Dual (arrInv g f)


-----------------------------------------------------------------------------
-- Instances : Comparable

instance (Arbitrary a, Show a, Comparable (m b)) => Comparable (Kleisli m a b) where
      Kleisli f `equals` Kleisli g = property $ \x -> f x `equals` g x

instance Comparable (cat a b) => Comparable (Env cat a b) where
      Env f `equals` Env g = f `equals` g

instance Comparable (f (cat a b)) => Comparable (Static f cat a b) where
      Static f `equals` Static g = f `equals` g

instance Comparable (cat b a) => Comparable (Dual cat a b) where
      Dual f `equals` Dual g = f `equals` g

-----------------------------------------------------------------------------
-- Instances : Arbitrary

instance (Arbitrary a, Arbitrary (m b)) => Arbitrary (Kleisli m a b) where
      arbitrary = liftM Kleisli arbitrary
      coarbitrary (Kleisli c) = coarbitrary c

instance Arbitrary (cat a b) => Arbitrary (Env cat a b) where
      arbitrary = liftM Env arbitrary
      coarbitrary (Env c) = coarbitrary c

instance Arbitrary (f (cat a b)) => Arbitrary (Static f cat a b) where
      arbitrary = liftM Static arbitrary
      coarbitrary (Static c) = coarbitrary c

instance Arbitrary (cat b a) => Arbitrary (Dual cat a b) where
      arbitrary = liftM Dual arbitrary
      coarbitrary (Dual c) = coarbitrary c
