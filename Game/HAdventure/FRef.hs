module Game.HAdventure.FRef where

data FRef s a = FRef
    { get :: s -> a
    , set :: a -> s -> s
    }

update :: FRef s a -> (a -> a) -> (s -> s)
update ref f s = set ref (f (get ref s)) s

compose :: FRef b c -> FRef a b -> FRef a c
compose bc ab = FRef
      { get = get bc . get ab
      , set = update ab . set bc
      }

class Ref r where
    ref :: (a -> b) -> (b -> a -> a) -> r a b
    (.@.) :: r b c -> r a b -> r a c

instance Ref FRef where
    ref = FRef
    (.@.) = compose

instance Ref (->) where
    ref = const
    (.@.) = (.)