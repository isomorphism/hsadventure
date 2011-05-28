{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Control.Category.Properties
-- Copyright   : (c) 2007 Twan van Laarhoven
-- License     : BSD-style
-- Maintainer  : twanvl@gmail.com
-- Stability   : experimental
-- Portability : non-portable (flexible instances, scoped typevars)
-- 
-- QuickCheck properties for verifying that a given category satisfies the laws of a particular class
--
-- For example, to verify that @MyCat@ satisfies the 'Category' laws, use:
--
-- > mapM_ test (props_Category (id :: MyCat Int Int))
--
-- The @id@ is just a tag to tell the type checker what category you want to test
-----------------------------------------------------------------------------
module Control.Category.Properties
        ( -- * Properties
          props_Category
        , props_InvArrow
        , props_CategoryPair
        , props_CategoryFanOut
        , props_CategorySelect
        , props_CategoryFanOut_Select
        , props_CategoryChoice
        , props_CategoryFanIn
        , props_CategoryInject
          -- * Utility
        , Comparable(..)
        ) where

import Test.QuickCheck
import Text.Show.Functions ()
import qualified Prelude as P
import Prelude          ( Eq(..), Show(..)
                        , ($), asTypeOf
                        , Int, Bool(..), Char, Either
                        )

import Control.Category
import Control.Category.Internal.Comparable
import Data.Invertible

-----------------------------------------------------------------------------
-- Utility instances

instance Show (Invertible a b) where
      show _ = "<Invertible>"

-----------------------------------------------------------------------------
-- Basic categories

-- | Tests that @cat@ satisfies the 'Category' laws.
props_Category :: (Category cat, Comparable (cat a a), Show (cat a a))
               => cat a a -> [Property]
props_Category _tag =
      [ label "Left Identity:  id . f == f"                $ \f -> id . f `equals` (f `asTypeOf` _tag)
      , label "Right Identity: f . id == f"                $ \f -> f . id `equals` (f `asTypeOf` _tag)
      , label "Associativity:  f . (g . h) == (f . g) . h" $ \f g h -> f . (g . h) `equals` (f . g) . (h `asTypeOf` f `asTypeOf` g `asTypeOf` _tag)
      ]

-- | Tests that @cat@ satisfies the 'InvArrow' laws.
--   Does /not/ test 'Category' laws.
props_InvArrow :: (InvArrow cat, Comparable (cat a a), Show (cat a a), Arbitrary a, Eq a)
               => cat a a -> [Property]
props_InvArrow _tag =
      [ label "arrInv id:      arrInv id id == id"            $ arrInv' id id `equals` id
      , label "arrInv (.):     arrInv f g . arrInv h i == arrInv (f . h) (i . g)"
                                                              $ \(Invertible f g) (Invertible h i) -> arrInv' f g . arrInv' h i `equals` arrInv' (f . h) (i . g)
      , label "arrInv inverse: arrInv f g . arrInv g f == id" $ \(Invertible f g) -> arrInv' f g . arrInv' g f `equals` id
      ]
 where arrInv' f g = arrInv f g `asTypeOf` _tag

-----------------------------------------------------------------------------
-- Pairs

-- | Tests that @cat@ satisfies the 'CategoryPair' laws.
--   Does /not/ test 'Category' laws.
props_CategoryPair :: forall cat a.
                      ( CategoryPair cat
                      , Comparable (cat (a,a) (a,a))
                      , Arbitrary (cat a a)
                      , Show (cat a a))
                   => cat a a -> [Property]
props_CategoryPair _tag =
      [ label "Swap Inverse:   swap . swap == id"                  $ swap' . swap' `equals` id
      , label "First id:       first id == id"                     $ first id `equals` (id `asTypeOf` swap')
      , label "First compose:  first f . first g == first (f . g)" $ \f g -> first f . first g `equals` first (f . (g `asTypeOf` f `asTypeOf` _tag)) `asTypeOf` swap'
      , label "First/Second:   swap . first f = second f . swap"   $ \f -> swap' . first f `equals` second f . swap'
      ]
 where swap' = swap :: cat (a,a) (a,a)
{-
                         , label "***:          f *** g = second g . first f"     $ \f g -> f *** g `equals` (first f `asTypeOf` _tag) . (second g `asTypeOf` _tag)
                         , label "***:          f *** g = first f . second g"     $ \f g -> f *** g `equals` (first f `asTypeOf` _tag) . (second g `asTypeOf` _tag)
                         ]
-}

-- | Tests that @cat@ satisfies the 'CategoryFanOut' laws.
--   Does /not/ test 'CategoryPair' or 'Category' laws.
props_CategoryFanOut :: forall cat a.
                      ( CategoryFanOut cat
                      , Comparable (cat a (a,a))
                      , Arbitrary (cat a a)
                      , Show (cat a a))
                   => cat a a -> [Property]
props_CategoryFanOut _tag =
      [ label "FanOut id:      id &&& id == dup"                    $ id' &&& id' `equals` dup
      -- , label "WRONG:          f &&& g == swap . (g &&& f)"         $ \f g -> f &&& g `equals` swap . (g &&& f :: cat a (a,a))
      ]
 where id' = id `asTypeOf` _tag

-- | Tests that @cat@ satisfies the 'CategorySelect' laws.
props_CategorySelect :: forall cat a.
                      ( CategorySelect cat
                      , Comparable (cat (a,a) a)
                      , Arbitrary (cat a a)
                      , Show (cat a a))
                   => cat a a -> [Property]
props_CategorySelect _tag =
      [ label "Fst/first:      fst . first f == f . fst"             $ \f -> fst' . first f `equals` f . fst'
      , label "Fst/Snd:        fst == snd . swap"                    $ fst' `equals` snd . swap
      ]
 where fst' = fst :: cat (a,a) a


-- | Tests that @cat@ satisfies the 'CategoryFanOut'+'CategorySelect' laws.
props_CategoryFanOut_Select :: forall cat a.
                      ( CategoryFanOut cat, CategorySelect cat
                      , Comparable (cat (a,a) (a,a)))
                   => cat a a -> [Property]
props_CategoryFanOut_Select _tag =
      [ label "FanOut id:      fst &&& snd == id"                    $ fst &&& snd `equals` id'
      ]
 where id' = id :: cat (a,a) (a,a)


-----------------------------------------------------------------------------
-- Choice

-- | Tests that @cat@ satisfies the 'CategoryChoice' laws.
--   Does /not/ test 'Category' laws.
props_CategoryChoice :: forall cat a.
                        ( CategoryChoice cat
                        , Comparable (cat (Either a a) (Either a a))
                        , Arbitrary (cat a a)
                        , Show (cat a a))
                     => cat a a -> [Property]
props_CategoryChoice _tag =
      [ label "Mirror Inverse: mirror . mirror == id"              $ mirror' . mirror' `equals` id
      , label "Left id:        left id == id"                      $ left id `equals` (id `asTypeOf` mirror')
      , label "Left compose:   left f . left g == left (f . g)"    $ \f g -> left f . left g `equals` left (f . (g `asTypeOf` f `asTypeOf` _tag)) `asTypeOf` mirror'
      , label "Left/Right:     mirror . left f = right f . mirror" $ \f -> mirror' . left f `equals` right f . mirror'
      ]
 where mirror' = mirror :: cat (Either a a) (Either a a)

-- | Tests that @cat@ satisfies the 'CategoryFanIn' laws.
--   Does /not/ test 'CategoryChoice' or 'Category' laws.
props_CategoryFanIn :: forall cat a.
                      ( CategoryFanIn cat
                      , Comparable (cat (Either a a) a)
                      , Arbitrary (cat a a)
                      , Show (cat a a))
                   => cat a a -> [Property]
props_CategoryFanIn _tag =
      [ label "FanIn id:       id ||| id == untag"                 $ id' ||| id' `equals` untag
      ]
 where id' = id `asTypeOf` _tag

-- | Tests that @cat@ satisfies the 'CategoryInject' laws.
props_CategoryInject :: forall cat a.
                      ( CategoryInject cat
                      , Comparable (cat a (Either a a))
                      , Arbitrary (cat a a)
                      , Show (cat a a))
                   => cat a a -> [Property]
props_CategoryInject _tag =
      [ label "inl/left:       left f . inl == inl . f"                $ \f -> left f . inl' `equals` inl' . f
      , label "inl/inr:        inl == mirror . inr"                    $ inl' `equals` mirror . inr
      ]
 where inl' = inl :: cat a (Either a a)
