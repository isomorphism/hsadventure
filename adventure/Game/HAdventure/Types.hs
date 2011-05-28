{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses #-}
module Game.HAdventure.Types (

  -- * Basic types

  FRef, update, set,

  ObjectType(..),
  ActionMap,
  ActionList,
  Object(..),
  name, description, objType, actions, children, aliases, takeable, hidden,

  Thing, Location, Character,

  Direction(..),
  dirToStr,

  Score,
  Command(..),

  emptyThing,
  emptyLoc,
  emptyCharacter,

  -- * The GameState type

  Target(..), handleTarget, targetToMaybe,
  GameState(..),
  score, dirO, indO, loc, childMap, visited, globalActions, config,

  emptyGameState,
  inventoryID,

  -- * The GameConfig type

  GameConfig(..),
  newGlobalActions, overrideGlobalActions, initialInventory, startLoc,
  emptyConfig,
  applyConfig,

  -- * Utilities

  inventory,
  modifyInv,
  addObject,
  addObjectHere,
  delObject,
  delObjectHere,
  visit,
  addGlobalActions,

  -- * The Adv monad

  Adv(..),
  MonadAdvCtrl(..),

  runAdv,
  io,
  setS, updateS

) where

import Control.Monad.State
import Control.Monad.Maybe
import qualified Data.Map as M
import Data.List (delete)
import Data.Maybe (fromMaybe, fromJust)
import Control.Applicative ((<$>))

import Data.FRef hiding (get)
import Data.FRef.Derive

-- | Directions in which the player can travel between locations.
data Direction = N    -- ^ north
               | S    -- ^ south
               | E    -- ^ east
               | W    -- ^ west
               | NE   -- ^ northeast
               | SE   -- ^ southeast
               | SW   -- ^ southwest
               | NW   -- ^ northwest
               | D    -- ^ down
               | U    -- ^ up
               | I    -- ^ in
               | O    -- ^ out
  deriving (Read,Show,Eq,Ord,Enum)

-- | Convert a 'Direction' to a 'String'.
dirToStr :: Direction -> String
dirToStr N  = "north"
dirToStr S  = "south"
dirToStr E  = "east"
dirToStr W  = "west"
dirToStr NE = "northeast"
dirToStr SE = "southeast"
dirToStr SW = "southwest"
dirToStr NW = "northwest"
dirToStr D  = "down"
dirToStr U  = "up"
dirToStr I  = "in"
dirToStr O  = "out"

-- | The type by which the player's score is represented.
type Score = Int

-- | A type to encapsulate commands issued by the player.
data Command = Cmd String | Dir Direction
  deriving (Read,Show,Eq,Ord)

type ActionMap = M.Map Command (Adv ())
type ActionList = [(Command, Adv ())]

-- | A tag to denote different kinds of 'Object's.
data ObjectType = Location | Thing | Character

-- | The basic building block of adventure worlds.  'Object's are used
--   to represent locations, things, and characters.
--
--   'Object's are keyed on the 'name' field, which is used to
--   implement both equality and ordering for 'Object's.
data Object = Obj { name_        :: String
                  , description_ :: String
                  , objType_     :: ObjectType
                  , actions_     :: ActionMap
                  , children_    :: [Object]
                  , aliases_     :: [String]
                  , takeable_    :: Bool
                  , hidden_      :: Bool
                  }

-- | A default, empty object.
emptyThing :: Object
emptyThing = Obj { name_        = ""
                 , description_ = ""
                 , objType_     = Thing
                 , actions_     = M.empty
                 , children_    = []
                 , aliases_     = []
                 , takeable_    = True
                 , hidden_      = False
                 }

-- | A convenience alias.  At some point in the future, the 'Object' type
--   may get tagged with a phantom type so these types can be distinguished
--   at compile-time.
type Thing = Object

-- | Convenience alias for 'Object'.  See note about phantom types above.
type Location = Object

-- | Convenience alias for 'Object'.  See note about phantom types above.
type Character = Object

data Target a = Target a
              | Unknown String
              | None

handleTarget :: b -> (String -> b) -> (a -> b) -> Target a -> b
handleTarget b _ _ None        = b
handleTarget _ f _ (Unknown s) = f s
handleTarget _ _ f (Target a)  = f a

targetToMaybe :: Target a -> Maybe a
targetToMaybe = handleTarget Nothing (const Nothing) Just

instance Functor Target where
  fmap f (Target x)  = Target (f x)
  fmap _ (Unknown s) = Unknown s
  fmap _ None        = None

-- | A record type to store the game's persistent (mutable) state.
data GameState = G { score_          :: Score         -- ^ current score
                   , dirO_           :: Target Object -- ^ direct object
                   , indO_           :: Target Object -- ^ indirect object
                   , loc_            :: Object        -- ^ current location
                   , childMap_       :: M.Map String [Object]
                       -- ^ a map with the current children of each object
                   , visited_        :: M.Map String Object
                       -- ^ visited locations
                   , globalActions_  :: ActionMap     -- ^ global actions
                   , config_         :: GameConfig
                       -- ^ configuration options
                   }

-- | Configuration record used for parameterizing a game.
data GameConfig = GC { newGlobalActions_      :: ActionMap
                     , overrideGlobalActions_ :: Bool
                     , initialInventory_      :: [Object]
                     , startLoc_              :: Object
                     }

data AdvCtrl a = Continue a
               | Exit
               | Fail

instance Functor AdvCtrl where
  fmap f (Continue x) = Continue (f x)
  fmap f Exit = Exit
  fmap f Fail = Fail

fromAdvCtrl :: a -> AdvCtrl a -> a
fromAdvCtrl _ (Continue x) = x
fromAdvCtrl x _ = x

newtype AdvCtrlT m a = AdvCtrlT { runAdvCtrlT :: m (AdvCtrl a) }

class (Monad m) => MonadAdvCtrl m where
  exit :: m ()
  cond :: Bool -> m ()
  try :: m a -> m ()

instance Functor m => Functor (AdvCtrlT m) where
  fmap f x = AdvCtrlT $ fmap (fmap f) $ runAdvCtrlT x

instance Monad m => Monad (AdvCtrlT m) where
  return  = AdvCtrlT . return . Continue
  x >>= f = AdvCtrlT $ do m <- runAdvCtrlT x
                          case m of
                            (Continue a) -> runAdvCtrlT (f a)
                            Fail         -> return Fail
                            Exit         -> return Exit
  fail _  = AdvCtrlT $ return Fail

instance (Monad m) => MonadAdvCtrl (AdvCtrlT m) where
  exit       = AdvCtrlT $ return Exit
  cond True  = return ()
  cond False = fail ""
  try  x     = AdvCtrlT $ do m <- runAdvCtrlT x
                             case m of
                               Continue _ -> return $ Continue ()
                               Fail       -> return $ Continue ()
                               Exit       -> return Exit

instance MonadTrans AdvCtrlT where
  lift m = AdvCtrlT $ m >>= return . Continue

instance MonadState s m => MonadState s (AdvCtrlT m) where
  get = lift get
  put = lift . put

instance MonadIO m => MonadIO (AdvCtrlT m) where
  liftIO = lift . liftIO

-- | The 'Adv' monad, which consists of a State monad of 'GameState'
--   layered on top of 'IO', topped off with a custom monad
--   transformer 'AdvCtrlT' expressing the necessary control flow.
newtype Adv a = Adv (AdvCtrlT (StateT GameState IO) a)
  deriving (Monad, Functor, MonadState GameState, MonadIO, MonadAdvCtrl)

-- | Run an action in the 'Adv' monad given an initial 'GameState',
--   resulting in an 'IO' action.
runAdv :: Adv () -> GameState -> IO ()
runAdv (Adv adv) gameState = fromAdvCtrl () <$> evalStateT (runAdvCtrlT adv) gameState

-- Note, because of this FRef deriving, the order of things in this
-- source file is important!  Most of the code below this point must
-- be there, because it depends on the code generated here.
$(deriveRefs ''Object)
$(deriveRefs ''GameState)
$(deriveRefs ''GameConfig)

instance Eq Object where
  o1 == o2 = (name o1) == (name o2)

instance Ord Object where
  o1 <= o2  = (name o1) <= (name o2)

-- | A default, empty location.
emptyLoc :: Object
emptyLoc = set objType Location emptyThing

-- | A default, empty character.
emptyCharacter :: Object
emptyCharacter = set objType Character emptyThing

-- | String ID for the special \'inventory\' object.
inventoryID :: String
inventoryID = "__INVENTORY"

-- | An empty game state.
emptyGameState :: GameState
emptyGameState = G { score_         = 0
                   , dirO_          = None
                   , indO_          = None
                   , loc_           = emptyLoc
                   , childMap_      = M.empty
                   , visited_       = M.empty
                   , globalActions_ = M.empty
                   , config_        = emptyConfig
                   }

-- | A default, empty game configuration.
emptyConfig :: GameConfig
emptyConfig = GC { newGlobalActions_      = M.empty
                 , overrideGlobalActions_ = False
                 , initialInventory_      = []
                 , startLoc_              = emptyLoc
                 }

applyConfig :: GameConfig -> Adv ()
applyConfig gc = do
  (if (overrideGlobalActions gc)
     then setS globalActions
     else addGlobalActions) $ newGlobalActions gc
  modifyInv (++ (initialInventory gc))
  setS config gc

setS :: (MonadState s m) => FRef s a -> a -> m ()
setS r x = modify (set r x)

updateS :: (MonadState s m) => FRef s a -> (a -> a) -> m ()
updateS r f = modify (update r f)

-- | Get the player's current inventory.
inventory :: Adv [Object]
inventory = (fromJust . M.lookup inventoryID) <$> gets childMap

-- | Modify the user's inventory with a function.
modifyInv :: ([Object] -> [Object]) -> Adv ()
modifyInv f = updateS childMap (M.adjust f inventoryID)

-- | Modify the child map by creating a new object with a given parent.
addObject :: Object    -- ^ The object to create.
          -> Object    -- ^ The parent object under which to create
                       --   the new object.
          -> Adv ()
addObject o l = updateS childMap (M.adjust (o:) (name l))

-- | Modify the child map by creating an object at the current
--   location.
addObjectHere :: Object -> Adv ()
addObjectHere o = gets loc >>= addObject o

-- | Modify the child map by deleting an object from a given parent.
delObject :: Object     -- ^ The object to delete.
          -> Object     -- ^ The parent object from which to delete.
          -> Adv ()
delObject o l = updateS childMap (M.adjust (delete o) (name l))

-- | Modify the child map by deleting an object from the current
--   location.
delObjectHere :: Object -> Adv ()
delObjectHere o = gets loc >>= delObject o

-- | Add a location to the 'visited' map.
visit :: Object -> Adv ()
visit o = updateS visited (M.insert (name o) o)

addGlobalActions :: ActionMap -> Adv ()
addGlobalActions acts = updateS globalActions (flip M.union acts)

-- XXX need to do this right.
--delGlobalActions ::

-- | Convenience alias for 'liftIO', to perform 'IO' actions in the
--   'Adv' monad.
io :: (MonadIO m) => IO a -> m a
io = liftIO

