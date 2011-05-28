module Game.HAdventure.Engine (

  -- XXX TODO: add documentation

  msg, msg',

  describeCurLoc,
  describeLoc,
  describeObj,
  describeChildren,

  curObjectList,
  lookupChildren,

  stay,
  storeObjectsOnFirstVisit,
  prompt,
  parseObjects,
  lookupObject,
  lookupObjectByName,

  badCmd,

  mkActionMap,

  ambigMsg,

  look,
  pp,
  rle,
  showScore,
  showInventory,

  doTake,
  addToInventory,
  doDrop,
  dropFromInventory,
  showCurrentCommands,

  withReadline

) where

import Game.HAdventure.Types

import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe
import Control.Applicative
import Control.Arrow ((&&&))
import Data.List
import Data.Char

import System.Console.SimpleLineEditor

import Text.PrettyPrint.HughesPJ

-- TODO: reorganize!

-- XXX paragraph width should be configurable?
-- | Display a nicely formatted message to the player.
msg :: String -> Adv ()
msg = io . putStrLn . pp 80

msg' :: String -> Adv ()
msg' = io . putStr . pp 80

-- XXX don't print description after first visit?
--   need to think carefully about how to do this.
-- | Describe the current location to the player: show its name,
--   description, and a list of any (non-hidden) 'Object's.
describeCurLoc :: Adv ()
describeCurLoc = do l <- gets loc
                    msg $ name l
                    describeLoc l

-- | Describe a location to the player: show its description and
--   a list of any (non-hidden) 'Object's at this location.
describeLoc :: Object -> Adv ()
describeLoc l = describeObj l >> describeChildren l

-- | Describe an 'Object' to the player by showing its description, or
--   a default message if the description is empty.
describeObj :: Object -> Adv ()
describeObj o = msg (if null (description o)
                       then "It's...not all that special."
                       else description o)

-- XXX better pluralization.
-- XXX parameter saying whether to show hidden objects?
-- XXX phrases for listing should be configurable?  Might want to use
--     this to describe children of non-location objects as well
--     (e.g. the contents of a bag).
-- | List the (non-hidden) children of an 'Object'.
describeChildren :: Object -> Adv ()
describeChildren o = mapM_ msg . process =<< lookupChildren o
  where process = map sayHere . rle . filter (not . hidden)
        sayHere (c,1) = "There is a " ++ (name c) ++ " here."
        sayHere (c,n) = "There are " ++ (show n) ++ " " ++ (name c) ++ "s here."

-- | Get a list of all the children of the current location.
curObjectList :: Adv [Object]
curObjectList = gets loc >>= lookupChildren

-- | Get a list of all the children of the given 'Object'.
lookupChildren :: Object -> Adv [Object]
lookupChildren o = do cm <- gets childMap
                      return (fromMaybe [] $ M.lookup (name o) cm)

-----------------------------------------------------------------------
-- main REPL, parsing, etc.
-----------------------------------------------------------------------

-- XXX refactor into more modular form!  Needed for implementing 'doGo'.
-- | The main show: display a prompt to the user, get their input, and
--   run the appropriate continuation action.
stay :: Adv ()
stay = do
    prompt
    tokens <- maybe [] tokenise <$> (io $ getLineEdited "")  -- XXX prompt
    if null tokens
      then stay
      else processCmdLine tokens

-- XXX TODO: redo with prompt monad! =)
-- | Display a nice prompt.
prompt :: Adv ()
prompt = io $ putStr "> "

tokenise :: String -> [String]
tokenise = filter (`notElem` smallWords) . words . map toLower

-- XXX make this configurable?
smallWords :: [String]
smallWords = words "at to with in on of for about"

processCmdLine :: [String] -> Adv ()
processCmdLine (cmd:args) = do
  parseObjects args
  executeCmd (parseCommand cmd)

parseCommand :: String -> Command
parseCommand str =
  case (lookup str directionStrs) of
    Just d -> Dir d
    Nothing -> Cmd str

directionStrs :: [(String, Direction)]
directionStrs =
  ((concat [ [(take 1 ds, d), (ds, d)] | d <- [N .. O], let ds = dirToStr d ])
  \\ [("n", NE), ("n", NW), ("s", SE), ("s", SW), ("i",I), ("o",O), ("d",D), ("u",U)])
  ++ [("ne", NE), ("nw", NW), ("se", SE), ("sw", SW)
     ,("enter", I), ("exit", O)]

-- | Given a list of 'String's representing the space-separated words
--   from the player's input, figure out what direct and indirect
--   'Object's the player is possibly referring to, and store them in
--   the state.
parseObjects :: [String] -> Adv ()
parseObjects []      = setS dirO None >> setS indO None
parseObjects [x]     = do lookupTarget x >>= setS dirO
                          setS indO None
parseObjects (x:y:_) = do lookupTarget x >>= setS dirO
                          lookupTarget y >>= setS indO

lookupTarget :: String -> Adv (Target Object)
lookupTarget s = do inv     <- inventory
                    curObjs <- curObjectList
                    let d = lookupObject s [curObjs, inv]
                    return $ maybe (Unknown s) Target d

-- | Look up an 'Object' by name in a list of inventories.
lookupObject :: String -> [[Object]] -> Maybe Object
lookupObject s = lookupObjectByName s . concat

-- | Look up an 'Object' by name from among a list of objects.
lookupObjectByName :: String -> [Object] -> Maybe Object
lookupObjectByName n objs = find byName objs
  where byName o = (n `isPrefixOf` name o) ||
                   or (map (n `isPrefixOf`) $ aliases o)

executeCmd :: Command -> Adv ()
executeCmd c = do
  d <- targetToMaybe <$> gets dirO  -- XXX FIXME: targetToMaybe isn't right here,
  l <- gets loc                     -- want to take advantage of extra information in Target!
  ga <- gets globalActions
  fromMaybe (badCmd c >> stay) $           -- try, in order:
    msum [ lookupCmd c =<< actions <$> d   -- actions on d.o.
         , lookupCmd c .   actions  $  l   -- local actions
         , lookupCmd c ga ]                -- global actions

-- | Present a message to the user when they have issued an
--   unrecognized command.
badCmd :: Command -> Adv ()
badCmd (Dir d) = msg "You can't go that way."
badCmd _ = msg $ "I don't understand."

lookupCmd :: Command -> ActionMap -> Maybe (Adv ())
lookupCmd d@(Dir _) m = M.lookup d m
lookupCmd (Cmd cmd) m | length cmds == 0  = Nothing
                      | length cmds == 1  = M.lookup (Cmd (head cmds)) m
                      | otherwise         = Just $ ambigMsg cmds >> stay
  where cmds = [ c | (Cmd c) <- M.keys m, cmd `isPrefixOf` c ]

ambigMsg :: [String] -> Adv ()
ambigMsg cmds = msg $ "Perhaps you meant: " ++ strs
  where strs = intercalate " " cmds

-- | Visit an 'Object', and store its children in the 'Object' map if it
--   hasn't already been visited before.  In this way the implicit
--   (static) algebraic structure of the world is loaded lazily into
--   the 'Object' map so it can be modified by the player.
storeObjectsOnFirstVisit :: Adv ()
storeObjectsOnFirstVisit = do
    vMap <- gets visited
    l <- gets loc
    if M.member (name l) vMap
      then return ()
      else updateS childMap (M.insert (name l) (children l)) >> visit l

-- | Given a list of (Command, action) pairs, create a map that can be
--   used for looking up the actions corresponding to given commands.
--   Each action is stored under all the prefixes of the corresponding
--   textual command; actions corresponding to prefixes of more than
--   one command are replaced by a message informing the player of the
--   ambiguity.
mkActionMap :: ActionList -> ActionMap
mkActionMap = M.fromList

{-
doGo :: Adv ()
doGo = do d <- gets dirO
          case d of
            None ->        msg "Go where?"
            (Unknown s) -> maybe (msg "I don't understand where you want to go.")
                                 lookupCmd (lookup s directionStrs)
-}

-- | Describe the direct object to the player, or describe the current
--   location if they did not specify a direct object.
look :: Adv ()
look = do d <- gets dirO
          handleTarget describeCurLoc unknownObjMsg describeObj d

unknownObjMsg :: String -> Adv ()
unknownObjMsg s = msg $ noneHereMsg s

noneHereMsg :: String -> String
noneHereMsg s = "I don't see a " ++ s ++ " here." -- XXX grammar

pp :: Int -> String -> String
pp n = renderStyle (style {lineLength = n, ribbonsPerLine = 1}) .
  fsep . map text . words

-- | Display the player's score.
showScore :: Adv ()
showScore = do s <- gets score
               msg $ "You have a score of " ++ (show s) ++ "."

-- | Display the player's inventory.
showInventory :: Adv ()
showInventory =
    do i <- inventory
       if null i
           then msg "You are not carrying anything."
           else do msg "You are carrying:"
                   mapM_ (msg . sayItem) $ rle i
  where sayItem (c,1) = name c
        sayItem (c,n) = show n ++ " " ++ name c ++ "s"  -- XXX grammar

-- | Sort and then run-length encode a list.
rle :: (Ord a) => [a] -> [(a,Int)]
rle = map (head &&& length) . group . sort

-- | Perform a \'take\': remove the direct object from the current
--   location and add it to the player's inventory.  Display an error
--   message if the direct object is invalid, unspecified, or not
--   takeable.
doTake :: Adv ()
doTake = do d    <- gets dirO
            objs <- curObjectList
            doTake' d objs
  where
    doTake' None        _    = msg "Take what?"
    doTake' (Unknown s) _    = msg $ noneHereMsg s
    doTake' (Target o)  objs = case (o `elem` objs, takeable o) of
                                 (False, _) -> msg $ noneHereMsg (name o)
                                 (_, False) -> msg "You can't take that."
                                 _          -> addToInventory o

-- XXX to think about: does this preserve per-object state?
-- | Perform a \'take\': remove an object from the current
--   location and add it to the player's inventory.
addToInventory :: Object -> Adv ()
addToInventory o = do
  msg $ "You pick up the " ++ (name o) ++ "."
  delObjectHere o
  modifyInv (o:)

-- | Perform a \'drop\': remove the direct object from the player's
--   inventory and add it to the current location.  Do nothing if the
--   direct object is invalid or unspecified.
doDrop :: Adv ()
doDrop = do d <- gets dirO
            inv <- inventory
            doDrop' d inv
  where
    doDrop' None        _   = msg "Drop what?"
    doDrop' (Unknown s) _   = msg $ noneHereMsg s
    doDrop' (Target o)  inv = if o `elem` inv
                                then dropFromInventory o
                                else msg "You're not holding that."

-- | Perform a \'drop\': remove an object from the player's inventory,
--   add it to the current location, and display a message.
dropFromInventory :: Object -> Adv ()
dropFromInventory o = do msg $ "You drop the " ++ (name o) ++ "."  -- XXX grammar
                         modifyInv (delete o)
                         addObjectHere o

-- XXX  get other commands (location/ d.o.)
showCurrentCommands :: Adv ()
showCurrentCommands = do
  ga <- gets globalActions
  let cmds = [ c | (Cmd c) <- M.keys ga ]
  io $ mapM_ putStrLn cmds

-- | Bracket an IO action with operations to set up and tear down the
--   readline library.
withReadline :: IO () -> IO ()
withReadline = (initialise>>) . (>>restore)

