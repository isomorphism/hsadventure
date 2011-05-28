{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- Module      : Tests
-- Copyright   : (c) 2007 Twan van Laarhoven
-- License     : BSD-style
-- Maintainer  : twanvl@gmail.com
-- Stability   : experimental
-- Portability : non-portable (newtype deriving)
-- 
-- 
-- Tests for:
--
--  * Category laws of the types from this libary
--
--  * Functions from Invertible and FRef
--
-----------------------------------------------------------------------------
module Main where

import Test.QuickCheck
import Control.Monad    (liftM)

import Control.Category (Kleisli, Static, Env, Dual(..))
import qualified Control.Category as C
import Control.Category.Properties
import Data.Invertible
import Data.FRef

-----------------------------------------------------------------------------
-- Utility instances

instance Show (FRef a b) where
      show _ = "<FRef>"
instance Show (Kleisli m a b) where
      show _ = "<Kleisli>"

-----------------------------------------------------------------------------
-- Main function

main = do
        putStrLn "Testing: Invertible functions"
        test prop_inverse1
        test prop_inverse2
        test prop_call_inverse1
        test prop_call_inverse2
        test prop_inverse_id
        
        putStrLn "Testing: FRef functions"
        test prop_get_set
        test prop_update_id
        test prop_set_get
        test prop_update_get
        test prop_set_set
                
        putStrLn "Testing: Invertible laws"
        testLaws (C.id :: Invertible a a) 
            [ props_Category
            , props_InvArrow
            , props_CategoryPair
            , props_CategoryChoice
            ]
        
        putStrLn "Testing: FRef laws"
        testLaws (C.id :: FRef a a)
            [ props_Category
            , props_InvArrow
            , props_CategoryPair
            , props_CategoryFanOut
            , props_CategorySelect
            , props_CategoryFanOut_Select
            , props_CategoryChoice
            , props_CategoryFanIn
            , props_CategoryInject
            ]
        
        putStrLn "Testing: (->) laws"
        testLaws (C.id :: a -> a)
            [ props_Category
            , props_InvArrow
            , props_CategoryPair
            , props_CategoryFanOut
            , props_CategorySelect
            , props_CategoryFanOut_Select
            , props_CategoryChoice
            , props_CategoryFanIn
            , props_CategoryInject
            ]
        
        putStrLn "Testing: (Kleisli []) laws"
        testLaws (C.id :: Kleisli [] a a)
            [ props_Category
            , props_InvArrow
            , props_CategoryPair
            , props_CategoryFanOut
            , props_CategorySelect
            , props_CategoryFanOut_Select
            , props_CategoryChoice
            , props_CategoryFanIn
            , props_CategoryInject
            ]
        
        putStrLn "Testing: (Kleisli (Env (->) _)) laws"
        testLaws (C.id :: Kleisli (Env (->) Bool) a a)
            [ props_Category
            , props_InvArrow
            , props_CategoryPair
            , props_CategoryFanOut
            , props_CategorySelect
            , props_CategoryFanOut_Select
            , props_CategoryChoice
            , props_CategoryFanIn
            , props_CategoryInject
            ]
        
        putStrLn "Testing: (Static [] (->)) laws"
        testLaws (C.id :: Static [] (->) a a)
            [ props_Category
            , props_InvArrow
            , props_CategoryPair
            , props_CategoryFanOut
            , props_CategorySelect
            , props_CategoryFanOut_Select
            , props_CategoryChoice
            , props_CategoryFanIn
            , props_CategoryInject
            ]
        
        putStrLn "Testing: (Dual Invertible) laws"
        testLaws (C.id :: Dual Invertible a a)
            [ props_Category
            , props_InvArrow
            , props_CategoryPair
            , props_CategoryChoice
            ]
        
        putStrLn "Testing: (Dual (->)) laws"
        testLaws (C.id :: Dual (->) a a)
            [ props_Category
            , props_InvArrow
            , props_CategoryPair
            , props_CategoryChoice
            ]

-----------------------------------------------------------------------------
-- Functions : Invertible

prop_inverse1, prop_inverse2 :: Invertible Int Int -> Property
prop_inverse1 f = label "f . inverse f == id" $ f C.. inverse f `equals` C.id
prop_inverse2 f = label "inverse f . f == id" $ inverse f C.. f `equals` C.id

prop_call_inverse1, prop_call_inverse2 :: Invertible Int Int -> Int -> Property
prop_call_inverse1 f x = label "call f . inverse f == id" $ call f (inverse f x) == x
prop_call_inverse2 f x = label "inverse f . call f == id" $ inverse f (call f x) == x

prop_inverse_id :: Property
prop_inverse_id = label "inverse id == id" $ inverse C.id `equals` (C.id :: Invertible Int Int)

-----------------------------------------------------------------------------
-- Functions : FRef

newtype Structure = Structure Int
      deriving (Arbitrary, Eq, Show)
newtype Member = Member Int
      deriving (Arbitrary, Eq, Show)

prop_get_set, prop_update_id :: FRef Structure Member -> Structure -> Property
prop_get_set   fr s = label "'set . get' ~= id" $ set fr (get fr s) s == s
prop_update_id fr s = label "update id == id"   $ update fr id s == s

prop_set_get :: FRef Structure Member -> Structure -> Member -> Property
prop_set_get fr s m = label "'get . set x' ~= x" $ get fr (set fr m s) == m

prop_update_get :: FRef Structure Member -> Structure -> (Member -> Member) -> Property
prop_update_get fr s f = label "get . update f == f . get" $ get fr (update fr f s) == f (get fr s)

prop_set_set :: FRef Structure Member -> Structure -> Member -> Property
prop_set_set fr s m = label "'set . set' ~= set" $ set fr m (set fr m s) == set fr m s

-----------------------------------------------------------------------------
-- Category laws

testLaws :: cat Int Int -> [cat Int Int -> [Property]] -> IO ()
testLaws tag laws = mapM_ test (concatMap ($tag) laws)
