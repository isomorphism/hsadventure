{-# LANGUAGE UndecidableInstances #-}
module Game.HAdventure.Prompt where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans

class (Monad m) => MonadPrompt p m | m -> p where
  prompt :: p a -> m a

data Prompt (p :: * -> *) :: * -> * where
  PromptDone :: result -> Prompt p result
  Prompt     :: p a -> (a -> Prompt p result) -> Prompt p result

instance Functor (Prompt p) where
  fmap f (PromptDone r)   = PromptDone (f r)
  fmap f (Prompt pa cont) = Prompt pa (fmap f . cont)

instance Monad (Prompt p) where
  return r = PromptDone r
  (PromptDone r) >>= f = f r
  (Prompt pa c)  >>= f = Prompt pa ((>>= f) . c)

instance MonadPrompt p (Prompt p) where
  prompt pa = Prompt pa return

-- ??? is this right?
instance (MonadPrompt p m, MonadTrans t, Monad (t m), MonadState s m) => MonadState s (t m) where
  get = lift get
  put = lift . put


runPromptM :: (Monad m) => (forall a. p a -> m a) -> Prompt p r -> m r
runPromptM _ (PromptDone r) = return r
runPromptM f (Prompt pa c)  = f pa >>= runPromptM f . c

runPrompt :: (forall a. p a -> a) -> Prompt p r -> r
runPrompt f = runIdentity . runPromptM (Identity . f)