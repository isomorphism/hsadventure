{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- Module      : DeriveExample
-- Copyright   : (c) 2007 Twan van Laarhoven
-- License     : BSD-style
-- Maintainer  : twanvl@gmail.com
-- Stability   : experimental
-- Portability : non-portable (template haskell)
-- 
-- An example of using "Data.FRef" and "Data.FRef.Derive"
--
-----------------------------------------------------------------------------
module Main where

import Control.Monad (liftM)

import Data.FRef
import Data.FRef.Derive

-----------------------------------------------------------------------------

-- | A very simple employee record,
--   note the underscores at the end of the fieldnames
data Employee = Employee
      { name_        :: String
      , salary_      :: Int
      , departments_ :: [String]
      }
     deriving (Show)

-- The magic:
$(deriveRefs ''Employee)

-- Demonstration
main = do
         -- Meet John
         let john = Employee "John" 1000 ["Accounting","IT"]
         print $ john
         -- Joe is just like John, but he is called "Joe" (obviously)
         print $ set name "Joe" john
         -- Let's give John a raise
         print $ update salary (*2) john
         -- At various times in the past, John had a different salary, the total record looked like
         mapM_ print $ setM salary [600,800,1000] john
         -- Interactively change John's salary
         print =<< setM salary (liftM read getLine) john
         -- At various times in the past, John had a different salary, the total record looked like
         mapM_ print $ updateM salary (\s -> [s*2,s*3,s*4]) john
         -- Change all the departments john works in
         print $ updateAll departments reverse john
