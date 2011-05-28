-----------------------------------------------------------------------------
-- |
-- Module      : Data.FRef.Derive
-- License     : BSD-style
-- Maintainer  : twanvl@gmail.com
-- Stability   : experimental
-- Portability : not portable (template haskell)
-- 
-- Derive functional reference functions for record fields.
--
-- Given the file:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import Data.FRef
-- > import Data.FRef.Derive
-- >
-- > data Foo a = Foo { x_ :: Int, y_ :: a }
--
-- the splice:
--
-- > $(deriveRefs ''Foo)
--
-- adds declarations with the types:
--
-- > x :: RefArrow r => r (Foo a) Int
-- > y :: RefArrow r => r (Foo a) a
--
-- These functions can be used:
--
--  * as regular selectors, @x :: Foo -> Int@,
--
--  * using the 'set' function, @set x :: Int -> Foo -> Foo@
--
--  * using the 'update' function, @update x :: (Int -> Int) -> Foo -> Foo@
--
--  * using the 'updateM' function, @updateM x :: Functor m => (Int -> m Int) -> Foo -> Foo@
--
-----------------------------------------------------------------------------
module Data.FRef.Derive (deriveRefs, makeRefs, RefArrow(..)) where

import Data.List
import Language.Haskell.TH

import Control.Category
import Data.FRef -- for haddock

-----------------------------------------------------------------------------

-- |
-- 'deriveRefs', when given a type name, splices in functional references
-- for all of that type's fields.  It detects fields that end in
-- underscore and generates refs whose names have the underscore dropped.
--
deriveRefs :: Name -> Q [Dec]
deriveRefs typeName = do 
    -- Given the record type's name, we use 'reify' to query for
    -- information about it.  Here, it must be a data declaration, and
    -- we extract its constructors, type parameters, and context.
    TyConI t@(DataD {}) <- reify typeName
    makeRefs t

{-
withRefs :: Q [Dec] -> Q [Dec]
withRefs declQuote = do
    decls <- declQuote
    let types = [ t | t@(DataD {}) <- decls ]
    liftM ((decls ++) . concat) (mapM makeRefs types)
-}

-- | Make reference functions for the field names in a record type declaration
makeRefs :: Dec -> Q [Dec]
makeRefs (DataD ctx typeName params constrs _) = do
    -- We pull out all the field names and types for the given type's
    -- record-style constructors which end with the underscore character.
    let fields = nub [ (n, t) | RecC _ conFields <- constrs,
                                (n, _, t)        <- conFields,
                                "_" `isSuffixOf` nameBase n ]
    
    -- Fully apply typeName to its parameters to get a concrete type.
    -- 'stype' is for 'structure type', as opposed to 'ftype', 'field type',
    -- used below.
    let stype = foldl AppT (ConT typeName) (map (\(PlainTV n) -> VarT n) params)
    
    -- Names to be used in code generation.
    let [ref,refClass] = map mkName ["arrRef'", "RefArrow"]
    [r,s,x] <- mapM newName ["r", "s", "x"]
    let r' = PlainTV r
    
    -- Generate the type signature and ref definition for the given
    -- field name and type.  This is mapped over all the fields.
    let refDecls (field_, ftype) = [sig, def] where
          field  = mkName (init (nameBase field_))  -- drop the underscore
          
          sig    = SigD field typ                   -- field :: <typ>
          typ    = ForallT (r':params) (refR:ctx) ty -- (<refR>, ...) => <ty>
--           refR   = ConT refClass `AppT` VarT r      -- RefArrow r
          refR   = ClassP refClass [VarT r]      -- RefArrow r
          ty     = VarT r `AppT` stype `AppT` ftype -- r stype ftype
          
          def    = ValD (VarP field) (NormalB body) []       -- field = <body>
          body   = VarE ref `AppE` VarE field_ `AppE` setter -- ref field_ <setter>
          setter = LamE [VarP x, VarP s] sBody               -- \x s -> <sBody>
          sBody  = RecUpdE (VarE s) [(field_, VarE x)]       -- s {field_ = x}
    
    return (concatMap refDecls fields)

makeRefs _ = fail "makeRefs must be given a data declaration"
