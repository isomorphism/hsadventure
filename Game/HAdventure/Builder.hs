{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Game.HAdventure.Builder (

  -- * Data types

  Direction(..),
  Object,
  hidden, takeable,

  -- * Object creation
  -- $objects

  location,
  thing,
  character,
  (@@@), withChildren,
  (///), withAliases,
  (!!!), withActions,
  (!+), (!-),
  Action(..),

  -- * Actions

  incScore,
  go, stay,
  msg,
  reveal,
  revealAt,
  destroy,
  destroyAt,
  moveTo,
  die,
  promptUser,

  showScore,
  showInventory,

  -- * Conditionals and dependencies

  Predicate(..),
  require, (==>), requireMsg, (?:),

  has,
  dirObjIs,
  indirObjIs,
  isHere,
  isAt,

  -- * Configuration and running

  GameConfig,
  emptyConfig,
  mkConfig,
  play,
  startAt

) where

import Game.HAdventure.Types
import Game.HAdventure.Engine

import Control.Monad.State
import qualified Data.Map as M
import Control.Arrow (second)
import Control.Applicative

import Data.List
import Data.Maybe
import Data.Char

import System.Console.SimpleLineEditor (getLineEdited)

-------------------------------------------------------
-----  Object Construction  ---------------------------
-------------------------------------------------------

{- $objects

   Foo bar.
-}

-- | Make a lowercase alias for a name with uppercase.
mkAlias :: String -> [String]
mkAlias n | all isLower n = []
          | otherwise = [map toLower n]

-- | Create a new location.
location :: String  -- ^ The name of the location (must be unique).
         -> String  -- ^ An (optional) longer description.
         -> Object
location n d = emptyLoc { _name        = n
                        , _description = d
                        , _takeable    = False
                        , _aliases     = mkAlias n
                        }

-- | Create a new thing.
thing :: String -- ^ The name of the thing (must be unique).
      -> String -- ^ An (optional) longer description.
      -> Object
thing n d = emptyThing { _name        = n
                       , _description = d
                       , _aliases     = mkAlias n
                       }

-- | Create a new character.
character :: String  -- ^ The name of the character (must be unique).
          -> String  -- ^ An (optional) longer description.
          -> Object
character n d = emptyCharacter { _name        = n
                               , _description = d
                               , _takeable    = False
                               , _aliases     = mkAlias n
                               }

infixl 4 !!!, `withActions`, @@@, `withChildren`, ///, `withAliases`
infixl 4 !+, !-

-- | Attach a list of actions to an object.
(!!!), withActions :: Object -> [ActionList] -> Object
o !!! acts = flip (modL actions) o $
               M.union (mkActionMap (concat acts))
withActions = (!!!)

-- | Attach a list of children (subobjects) to an object.
(@@@), withChildren :: Object -> [Object] -> Object
o @@@ chs = modL children (++chs) o
withChildren = (@@@)

-- | Specify a list of (not necessarily unique) aliases for an object.
--   The player will be able to use any of the aliases to refer to the
--   object (in addition to its unique name).
(///), withAliases :: Object -> [String] -> Object
o /// as = modL aliases (++as) o
withAliases = (///)

-- | Set an object flag to @True@. Currently, the only flags are
--   'hidden' and 'takeable'.
(!+) :: Object -> Object :-> Bool -> Object
o !+ flag = setL flag True o

-- | Set an object flag to @False@.
(!-) :: Object -> Object :-> Bool -> Object
o !- flag = setL flag False o

-------------------------------------------------------
-----  Actions  ---------------------------------------
-------------------------------------------------------

-- woohoo, multiple dispatch on types! =D

infixl 0 -->

-- | A class to allow expressing actions in a number of different
--   ways, using a special '-->' combinator.
--
--   The left hand side of '-->' can be a 'String' (which specifies
--   the command which will trigger the action), a list of 'String's
--   (a list of command aliases), or a 'Direction'.  The right-hand
--   side can be a normal action of type @'Adv' ()@ to be executed, or
--   an 'Object' which is interpreted as a location to 'go' to.

class Action a r where
  (-->)  :: a -> r -> ActionList

instance Action String (Adv ()) where
  s --> act = [(Cmd s, try act >> stay)]

instance Action [String] (Adv ()) where
  ss --> act = map (flip (,) (try act >> stay) . Cmd) ss

instance Action Direction (Adv ()) where
  d --> act = [(Dir d, try act >> stay)]

instance Action String Object where
  s --> o = [(Cmd s, go o)]

instance Action [String] Object where
  ss --> o = map (flip (,) (go o) . Cmd) ss

instance Action Direction Object where
  d --> o = [(Dir d, go o)]

-- XXX make an action to add sth to the player's inventory.

-- | Increase the player's score.  Of course, increasing by a negative
--   number is the same as decreasing.
incScore :: Int -> Adv ()
incScore s = updateS score (+s)

-- | Go to a new location.
go :: Object -> Adv ()
go o = setS loc o >> storeObjectsOnFirstVisit >> describeCurLoc >> stay

-- | Create a particular object at the player's current location.
reveal :: Object -> Adv ()
reveal = addObjectHere

-- | Create a particular object at a particular location.
revealAt :: Object   -- ^ The object to create.
         -> Object   -- ^ The location at which to create it.
         -> Adv ()
revealAt = addObject

-- | Remove a particular object from the player's current location AND
-- | from the player's inventory.
destroy :: Object -> Adv ()
destroy o = delObjectHere o >> modifyInv (delete o)
-- XXX is this really what I want here?

-- | Remove an object from a particular location.
destroyAt :: Object   -- ^ The object to destroy.
          -> Object   -- ^ The location at which to destroy it.
          -> Adv ()
destroyAt = delObject

-- | Move an object to a particular location.
moveTo :: Object    -- ^ The object to move.
       -> Object    -- ^ The location to move it to.
       -> Adv ()
moveTo o target = delObjectHere o >> addObject o target
-- XXX this isn't right.  What if the object isn't here?

-- | Start a text adventure game, using the given configuration.
--   Generally, the @main@ method for a text adventure might look
--   something like
--
-- > main = play cfg
-- >   where cfg = mkConfig ...
--
play :: GameConfig -> IO ()
play gc =
  withReadline $ runAdv (applyConfig gc >> go (getL startLoc gc)) initialGameState

-- | Start the game at a particular starting location.  The contents
--   of the @main@ method can be simply @'start' x@, where @x@ is the
--   location at which the game should start.  The 'play' function
--   provides more flexibility by taking a 'GameConfig' instead of a
--   location.
startAt :: Object -> IO ()
startAt l = play (mkConfig [] False [] l)

-- | Create a game configuration record, suitable for passing as an
--   argument to 'play'.
mkConfig :: [ActionList]  -- ^ A list of global actions, e.g. @[ foo --> bar, baz --> ...@
         -> Bool          -- ^ Should the global actions replace the built-
                          --   in ones?
         -> [Object]      -- ^ A list of objects to start out in the player's
                          --   inventory.
         -> Object        -- ^ The starting location.
         -> GameConfig
mkConfig al over inv start =
    GC { _newGlobalActions      = mkActionMap (concat al)
       , _overrideGlobalActions = over
       , _initialInventory      = inv
       , _startLoc              = start
       }

-- | Start over.
restart :: Adv ()
restart = do conf <- gets $ getL config
             put initialGameState
             applyConfig conf
             go (getL startLoc conf)

-- | Kill the player.
die :: Adv ()
die = msg "You have died." >> showScore >> exit

-- XXX refactor all prompt stuff.
promptUser :: String -> Adv String
promptUser p = do msg' (p++" ")
                  io $ (fromMaybe "") <$> getLineEdited ""

-------------------------------------------------------
-----  Dependencies  ----------------------------------
-------------------------------------------------------

-- XXX Better, but still not quite what I want.  I still want nice
-- multiway branching! Need to think hard how best to support this.
-- Leave it for now, come back later.  Need to move ahead with actual
-- features! =)

infixl 1 ==>
infixr 3 <&&>
infixr 2 <||>

-- XXX FIXME (documentation)
-- | A class for expressing predicate/result pairs.  Currently, the
--   left hand side of '==>' must be a predicate of type @'Adv' Bool@;
--   the right hand side may be either an action of type @'Adv' ()@,
--   or an 'Object', which is interpreted as an action to 'go' to that
--   'Object'.  In other words, if @loc@ is an 'Object', @req ==> loc@
--   and @req ==> go loc@ are equivalent, for convenience.

-- XXX FIXME: add documentation to everything below...

class Predicate p where
  notP    :: p -> p
  (<&&>)  :: p -> p -> p
  (<||>)  :: p -> p -> p
  test    :: p -> Adv Bool

instance Predicate (Adv Bool) where
  notP        = liftM not
  (<&&>)      = liftM2 (&&)
  (<||>)      = liftM2 (||)
  test        = id

instance Predicate Bool where
  notP        = not
  (<&&>)      = (&&)
  (<||>)      = (||)
  test        = return

require :: (Predicate p) => p -> Adv ()
require p = test p >>= cond

requireMsg :: (Predicate p) => p -> String -> Adv ()
requireMsg p err = test p >>= if' (return ()) (msg err >> fail err)

if' :: a -> a -> Bool -> a
if' t e b = if b then t else e

(==>)   :: (Predicate p) => p -> Adv () -> Adv ()
p ==> a = try $ require p >> a

(?:) :: Adv Bool -> String -> Adv ()
(?:) = requireMsg

-- | Create general conditional actions.  A conditional of the form
--
-- > cond [ p1 ==> act1
-- >      , p2 ==> act2
-- >      , ...
-- >      , pn ==> actn ]
--
-- will test the predicates @c1@, @c2@, ... in order.  As soon as
-- a predicate is found that holds, its corresponding action will be
-- performed, and no further predicates will be tested.  The special
-- predicate 'always' holds unconditionally and is useful as an \'else\'
-- case at the end of a 'cond'.
--
-- For the different sorts of things that are allowed to be in the place
-- of @act1@, @act2@, and so on, see the 'CondResult' class, which defines
-- the '==>' operator.

-- Need some sort of multiway branching combinator.
-- choose :: [Adv ()] -> Adv ()
-- choose = foldr (|||) (return ())

-- | A predicate to test whether the player possesses a certain
--   'Object' in their inventory.
has :: Object -> Adv Bool
has o = (o `elem`) <$> inventory

-- | A predicate to test whether the direct object of the current
--   player command is a certain 'Object'.
dirObjIs :: Object -> Adv Bool
dirObjIs o = maybe False (o ==) . targetToMaybe <$> gets (getL dirO)

-- | A predicate to test whether the indirect object of the current
--   player command is a certain 'Object'.
indirObjIs :: Object -> Adv Bool
indirObjIs o = maybe False (o ==) . targetToMaybe <$> gets (getL indO)

-- | A predicate to test whether a certain 'Object' is present in the
--   player's current location, /not/ including the player's inventory.
isHere :: Object -> Adv Bool
isHere o = (o `elem`) <$> curObjectList

-- | A predicate to test whether the player is currently at a certain
--   location.
isAt :: Object -> Adv Bool
isAt l = (l ==) <$> gets (getL loc)

-- XXX add 'confirm' action, and use for quit/restart
-- | A map of default game-wide commands.  These can be overridden or
--   extended by the game developer (either at compile- or run-time!).
defaultGlobalActions :: ActionMap
defaultGlobalActions = mkActionMap . concat $
  [ ["look","examine"] --> look
  , "quit"             --> msg "Goodbye!" >> exit  -- XXX FIXME
  , "restart"          --> restart
  , "score"            --> showScore
  , "inventory"        --> showInventory
  , ["take","get"]     --> doTake
  , "drop"             --> doDrop
  , "help"             --> msg "You can:" >> showCurrentCommands
  ]

initialGameState :: GameState
initialGameState =
    emptyGameState { _globalActions = defaultGlobalActions
                   , _childMap      = M.singleton inventoryID []
                   }
