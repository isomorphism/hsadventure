-----------------------------------------------------------------------------
-- |
-- Module      : Control.Category.Defaults
-- Copyright   : (c) 2007 Twan van Laarhoven
-- License     : BSD-style
-- Maintainer  : twanvl@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- This module provides default implementations of class members based on sub-classes.
--
-- For instance any type that is an instance of Arrow is automatically an instance of Category,
--  but Haskell doesn't allow us to express this.
-- To simplify the creation of instances this module can be used.
--
-- The functions are all named @function_SubClass@, meaning an implementation of @function@ for 
--  types in @SubClass@.
--
-----------------------------------------------------------------------------
module Control.Category.Defaults
        ( -- * Category
          id_InvArrow
        , id_Arrow
          -- * CategoryConst
        , const_RefArrow
        , const_Arrow
        
          -- * CategoryPair
        , swap_InvArrow
        , swap_Arrow
        , swap_FanOut
        , sss_FanOut
          -- * CategorySelect
        , fst_RefArrow
        , snd_RefArrow
        , fst_Arrow
        , snd_Arrow
          -- * CategoryFanOut
        , dup_Arrow
        
          -- * CategoryChoice
        , mirror_InvArrow
        , mirror_Arrow
        , mirror_FanIn
        , ppp_FanIn
          -- * CategoryInject
        , inl_RefArrow
        , inr_RefArrow
        , inl_Arrow
        , inr_Arrow
          -- * CategoryFanIn
        , untag_Arrow
        
          -- * Arrow..
        , arrInv_RefArrow
        , arrInv_Arrow
        , arrRef_Arrow
        ) where

import Control.Category hiding (fst, snd, (.), id)
import qualified Control.Category as C

-----------------------------------------------------------------------------

-- | Default 'id' implementation for 'InvArrow's, 'RefArrow's.
id_InvArrow :: InvArrow cat => cat a a
id_InvArrow = arrInv (\x -> x) (\x -> x)
-- | Default 'id' implementation for 'Arrow's.
id_Arrow :: Arrow cat => cat a a
id_Arrow = arr (\x -> x)


-- | Default 'const' implementation for 'RefArrow's.
const_RefArrow :: RefArrow cat => a -> cat b a
const_RefArrow a = arrRef (\b -> (a, \_ -> b))
-- | Default 'const' implementation for 'Arrow's.
const_Arrow :: Arrow cat => a -> cat b a
const_Arrow a = arr (\_ -> a)


-- | Default 'swap' implementation for 'InvArrow's, 'RefArrow's.
swap_InvArrow :: InvArrow cat => cat (a,b) (b,a)
swap_InvArrow = arrInv swap swap
-- | Default 'swap' implementation for 'Arrow's.
swap_Arrow :: Arrow cat => cat (a,b) (b,a)
swap_Arrow = arr swap

-- | Default 'swap' implementation for 'CategorySelect' and 'CategoryFanOut's.
swap_FanOut :: (CategorySelect cat, CategoryFanOut cat) => cat (a,b) (b,a)
swap_FanOut = C.snd &&& C.fst

-- | Default '***' implementation for 'CategoryInject' and 'CategoryFanIn's.
sss_FanOut :: (CategorySelect cat, CategoryFanOut cat) => cat a b -> cat c d -> cat (a,c) (b,d)
sss_FanOut f g = (f C.. C.fst) &&& (g C.. C.snd)

-- | Default 'fst' implementation for 'RefArrow's.
fst_RefArrow :: RefArrow cat => cat (a,b) a
fst_RefArrow = arrRef (\ ~(x,y) -> (x, \x' -> (x',y)))
-- | Default 'fst' implementation for 'Arrow's.
fst_Arrow :: Arrow cat => cat (a,b) a
fst_Arrow = arr fst

-- | Default 'fst' implementation for 'RefArrow's.
snd_RefArrow :: RefArrow cat => cat (a,b) b
snd_RefArrow = arrRef (\ ~(x,y) -> (y, \y' -> (x,y')))
-- | Default 'fst' implementation for 'Arrow's.
snd_Arrow :: Arrow cat => cat (a,b) b
snd_Arrow = arr snd

-- | Default 'dup' implementation for 'Arrow's.
dup_Arrow :: Arrow cat => cat a (a,a)
dup_Arrow = arr (\x -> (x,x))


-- | Default 'mirror' implementation for 'InvArrow's, 'RefArrow's and 'Arrow's.
mirror_InvArrow :: InvArrow cat => cat (Either a b) (Either b a)
mirror_InvArrow = arrInv mirror mirror
-- | Default 'mirror' implementation for 'Arrow's.
mirror_Arrow :: Arrow cat => cat (Either a b) (Either b a)
mirror_Arrow = arr mirror

-- | Default 'mirror' implementation for 'CategoryInject' and 'CategoryFanIn's.
mirror_FanIn :: (CategoryInject cat, CategoryFanIn cat) => cat (Either a b) (Either b a)
mirror_FanIn = inr ||| inl

-- | Default '+++' implementation for 'CategoryInject' and 'CategoryFanIn's.
ppp_FanIn :: (CategoryInject cat, CategoryFanIn cat) => cat a b -> cat c d -> cat (Either a c) (Either b d)
ppp_FanIn f g = (inl C.. f) ||| (inr C.. g)

-- | Default 'inl' implementation for 'RefArrow's.
inl_RefArrow :: RefArrow cat => cat a (Either a b)
inl_RefArrow = arrRef (\x -> (Left x, \x' -> case x' of
                                                 Left x'' -> x''
                                                 _        -> x))
-- | Default 'inl' implementation for 'Arrow's.
inl_Arrow :: Arrow cat => cat a (Either a b)
inl_Arrow = arr Left

-- | Default 'inr' implementation for 'RefArrow's.
inr_RefArrow :: RefArrow cat => cat b (Either a b)
inr_RefArrow = arrRef (\x -> (Right x, \x' -> case x' of
                                                 Right x'' -> x''
                                                 _         -> x))
-- | Default 'inr' implementation for 'Arrow's.
inr_Arrow :: Arrow cat => cat b (Either a b)
inr_Arrow = arr Right

-- | Default 'untag' implementation for 'Arrow's.
untag_Arrow :: Arrow cat => cat (Either a a) a
untag_Arrow = arr untag


-- | Default 'arrInv' implementation for 'RefArrow's.
arrInv_RefArrow :: RefArrow r => (a -> b) -> (b -> a) -> r a b
arrInv_RefArrow f g = arrRef (\x -> (f x, g))

-- | Default 'arrInv' implementation for 'Arrow's.
arrInv_Arrow :: Arrow r => (a -> b) -> (b -> a) -> r a b
arrInv_Arrow f _ = arr f

-- | Default 'arrRef' implementation for 'Arrow's.
arrRef_Arrow :: Arrow r => (a -> (b, b -> a)) -> r a b
arrRef_Arrow f = arr (fst . f)
