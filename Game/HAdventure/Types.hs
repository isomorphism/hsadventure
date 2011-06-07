{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Game.HAdventure.Types (

  -- * Basic types

  (:->)(..), getL, setL, modL,

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
  Game.HAdventure.Types.outputStrLn,
  Game.HAdventure.Types.outputStr,
  getLineEdited,
  setS, updateS

) where

import Control.Monad.State
import Control.Monad.Trans.Maybe()
import qualified Data.Map as M
import Data.Function
import Data.List (delete)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>))

import System.Console.Haskeline
import Data.Record.Label

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
data Object = Obj { _name        :: String
                  , _description :: String
                  , _objType     :: ObjectType
                  , _actions     :: ActionMap
                  , _children    :: [Object]
                  , _aliases     :: [String]
                  , _takeable    :: Bool
                  , _hidden      :: Bool
                  }

-- | A default, empty object.
emptyThing :: Object
emptyThing = Obj { _name        = ""
                 , _description = ""
                 , _objType     = Thing
                 , _actions     = M.empty
                 , _children    = []
                 , _aliases     = []
                 , _takeable    = True
                 , _hidden      = False
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
data GameState = G { _score          :: Score         -- ^ current score
                   , _dirO           :: Target Object -- ^ direct object
                   , _indO           :: Target Object -- ^ indirect object
                   , _loc            :: Object        -- ^ current location
                   , _childMap       :: M.Map String [Object]
                       -- ^ a map with the current children of each object
                   , _visited        :: M.Map String Object
                       -- ^ visited locations
                   , _globalActions  :: ActionMap     -- ^ global actions
                   , _config         :: GameConfig
                       -- ^ configuration options
                   }

-- | Configuration record used for parameterizing a game.
data GameConfig = GC { _newGlobalActions      :: ActionMap
                     , _overrideGlobalActions :: Bool
                     , _initialInventory      :: [Object]
                     , _startLoc              :: Object
                     }

data AdvCtrl a = Continue a
               | Exit
               | Fail

instance Functor AdvCtrl where
  fmap f (Continue x) = Continue (f x)
  fmap _ Exit = Exit
  fmap _ Fail = Fail

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
newtype Adv a = Adv (AdvCtrlT (StateT GameState (InputT IO)) a)
  deriving (Monad, Functor, MonadState GameState, MonadIO, MonadAdvCtrl)

-- | Run an action in the 'Adv' monad given an initial 'GameState',
--   resulting in an 'IO' action.
runAdv :: Adv () -> GameState -> IO ()
runAdv (Adv adv) gameState = runInputT defaultSettings $
                             fromAdvCtrl () <$> evalStateT (runAdvCtrlT adv) gameState

-- Note, because of this FRef deriving, the order of things in this
-- source file is important!  Most of the code below this point must
-- be there, because it depends on the code generated here.
mkLabels [''Object, ''GameState, ''GameConfig]

instance Eq Object where
  (==) = (==) `on` getL name

instance Ord Object where
  (<=) = (<=) `on` getL name

-- | A default, empty location.
emptyLoc :: Object
emptyLoc = setL objType Location emptyThing

-- | A default, empty character.
emptyCharacter :: Object
emptyCharacter = setL objType Character emptyThing

-- | String ID for the special \'inventory\' object.
inventoryID :: String
inventoryID = "__INVENTORY"

-- | An empty game state.
emptyGameState :: GameState
emptyGameState = G { _score         = 0
                   , _dirO          = None
                   , _indO          = None
                   , _loc           = emptyLoc
                   , _childMap      = M.empty
                   , _visited       = M.empty
                   , _globalActions = M.empty
                   , _config        = emptyConfig
                   }

-- | A default, empty game configuration.
emptyConfig :: GameConfig
emptyConfig = GC { _newGlobalActions      = M.empty
                 , _overrideGlobalActions = False
                 , _initialInventory      = []
                 , _startLoc              = emptyLoc
                 }

applyConfig :: GameConfig -> Adv ()
applyConfig gc = do
  (if getL overrideGlobalActions gc
     then setS globalActions
     else addGlobalActions) $ getL newGlobalActions gc
  modifyInv (++ (getL initialInventory gc))
  setS config gc

setS :: (MonadState s m) => s :-> a -> a -> m ()
setS r x = modify (setL r x)

updateS :: (MonadState s m) => s :-> a -> (a -> a) -> m ()
updateS r f = modify (modL r f)

-- | Get the player's current inventory.
inventory :: Adv [Object]
inventory = (fromJust . M.lookup inventoryID) <$> gets (getL childMap)

-- | Modify the user's inventory with a function.
modifyInv :: ([Object] -> [Object]) -> Adv ()
modifyInv f = updateS childMap (M.adjust f inventoryID)

-- | Modify the child map by creating a new object with a given parent.
addObject :: Object    -- ^ The object to create.
          -> Object    -- ^ The parent object under which to create
                       --   the new object.
          -> Adv ()
addObject o l = updateS childMap (M.adjust (o:) (getL name l))

-- | Modify the child map by creating an object at the current
--   location.
addObjectHere :: Object -> Adv ()
addObjectHere o = gets (getL loc) >>= addObject o

-- | Modify the child map by deleting an object from a given parent.
delObject :: Object     -- ^ The object to delete.
          -> Object     -- ^ The parent object from which to delete.
          -> Adv ()
delObject o l = updateS childMap (M.adjust (delete o) (getL name l))

-- | Modify the child map by deleting an object from the current
--   location.
delObjectHere :: Object -> Adv ()
delObjectHere o = gets (getL loc) >>= delObject o

-- | Add a location to the 'visited' map.
visit :: Object -> Adv ()
visit o = updateS visited (M.insert (getL name o) o)

addGlobalActions :: ActionMap -> Adv ()
addGlobalActions acts = updateS globalActions (flip M.union acts)

-- | Get a haskeline edited prompt
getLineEdited :: String -> Adv (Maybe String)
getLineEdited = Adv . lift . lift . getInputLine

-- | A lifted version of outputStrLn
outputStrLn :: String -> Adv ()
outputStrLn = Adv . lift . lift . System.Console.Haskeline.outputStrLn

-- | A lifted version of outputStr
outputStr :: String -> Adv ()
outputStr = Adv . lift . lift . System.Console.Haskeline.outputStr