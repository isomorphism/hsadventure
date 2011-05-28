module Adventure where

{- TODO:

* break up into modules.
* use readline library.
* do command prefix matching.
* create darcs repo.
* haddock-ize.
* put objects in Map for easy changing.
* explicitly data-ize location links?
* need to support dependencies for taking, moving, etc.
  - a little DSL for expressing dependencies.

-}

import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Arrow ((***))
import qualified Data.Map as M
import Data.Maybe
import Data.List

type Score = Int
type Inventory = [Object]

data GameState = G { score :: Score         -- ^ current score
                   , dirO  :: Maybe Object  -- ^ direct object
                   , indO  :: Maybe Object  -- ^ indirect object
                   , loc   :: Object        -- ^ current location
                   , inventory :: Inventory -- ^ inventory
                   }

initialGameState = G { score = 0
                     , dirO = Nothing
                     , indO = Nothing
                     , loc  = emptyLoc
                     , inventory = []
                     }

newtype Adv a = Adv (StateT GameState IO a)
  deriving (Monad, Functor, MonadState GameState, MonadIO)

runAdv :: Adv a -> GameState -> IO a
runAdv (Adv adv) gameState = evalStateT adv gameState

io = liftIO

setLoc :: Object -> Adv ()
setLoc o = modify (\s -> s{ loc = o })

setDirO :: Maybe Object -> Adv ()
setDirO o = modify (\s -> s{ dirO = o })

setIndO :: Maybe Object -> Adv ()
setIndO o = modify (\s -> s{ indO = o })

modifyInv :: (Inventory -> Inventory) -> Adv ()
modifyInv f = modify (\s -> s{ inventory = f (inventory s) })

msg :: String -> Adv ()
msg s = io (putStrLn s)

go :: Object -> Adv ()
go o = setLoc o >> describeLoc >> stay

describeLoc :: Adv ()
describeLoc = do loc <- gets loc
                 io $ putStrLn (description loc)
                 describeChildren loc

describeChildren :: Object -> Adv ()
describeChildren o = io . mapM_ putStrLn . map sayHere $ children o
  where sayHere c = "There is a " ++ (name c) ++ " here."

stay :: Adv ()
stay = do
    loc <- gets loc
    prompt
    tokens <- words <$> (io getLine)
    if null tokens
      then stay
      else do
        let (act:args) = tokens
        parseObjects args
        d <- gets dirO
        fromMaybe (bad act >> stay) $
          msum [ extLookup act specialActions      -- try special actions
               , extLookup act $ actions loc       -- local actions
               , extLookup act =<< actions <$> d ] -- actions on d.o.

play :: Object -> IO ()
play start = runAdv (go start) initialGameState

prompt :: Adv ()
prompt = io $ putStr "> "

-- XXX fixme: do command prefixes etc.
extLookup :: String -> M.Map String a -> Maybe a
extLookup = M.lookup

-- XXX fixme: more nuanced parsing.  "to" etc.
parseObjects :: [String] -> Adv ()
parseObjects []    = setDirO Nothing >> setIndO Nothing
parseObjects [x]   = do loc <- gets loc
                        inv <- gets inventory
                        setDirO (lookupObject x [children loc, inv])
                        setIndO Nothing
parseObjects [x,y] = do loc <- gets loc
                        inv <- gets inventory
                        setDirO (lookupObject x [children loc, inv])
                        setIndO (lookupObject y [children loc, inv])

lookupObject :: String -> [[Object]] -> Maybe Object
lookupObject s = lookupObjectByName s . concat

lookupObjectByName :: String -> [Object] -> Maybe Object
lookupObjectByName n objs = find ((n `isPrefixOf`) . name) objs

bad :: String -> Adv ()
bad c = io . putStrLn $ "I don't know how to " ++ c ++ "."

specialActions :: M.Map String (Adv ())
specialActions = M.fromList $
  [ ("look", look >> stay)
  , ("quit", msg "Goodbye!" >> return ())
  , ("score", gets score >>= showScore >> stay)
  , ("inventory", gets inventory >>= showInv >> stay)
  , ("take", doTake >> stay)
  ]

look :: Adv ()
look = gets dirO >>= \dir ->
       gets loc  >>= \loc ->
       io . putStrLn . description . fromJust $
         dir `mplus` Just loc

incScore :: Int -> Adv ()
incScore s = modify (\g -> g { score = s + score g })

showScore :: Score -> Adv ()
showScore s = io . putStrLn $
  "You have a score of " ++ (show s) ++ "."

showInv :: Inventory -> Adv ()
showInv i = io $ do
  putStrLn "You are carrying:"
  mapM_ (putStrLn . name) i

doTake :: Adv ()
doTake = gets dirO >>= maybe (msg "I don't see that here.")
                             maybeAddToInventory

maybeAddToInventory :: Object -> Adv ()
maybeAddToInventory o = if takeable o
                          then msg "Ok." >> modifyInv (o:)
                          else msg "You can't take that."

takeable :: Object -> Bool
takeable (Obj _ _ Thing _ _) = True
takeable _ = False

data ObjectType = Location | Thing | Character

data Object = Obj { name :: String
                  , description :: String
                  , objType :: ObjectType
                  , actions :: M.Map String (Adv ())
                  , children :: [Object]
                  }

emptyLoc = Obj "" "" Location M.empty []

data Direction = N | S | E | W

dirToStr :: Direction -> String
dirToStr N = "north"
dirToStr S = "south"
dirToStr E = "east"
dirToStr W = "west"

location :: String -> String -> [(Direction, Object)] -> [Object] -> Object
location n d links ch = Obj n d Location linkMap ch
  where linkMap = M.fromList $ map (dirToStr *** mkAdv) links
        mkAdv loc = go loc

thing :: String -> String -> Object
thing n d = Obj n d Thing M.empty []

thingWithActions :: String -> String -> [(String, Adv ())] -> Object
thingWithActions n d acts = Obj n d Thing (M.fromList acts) []

character :: String -> String -> Object
character n d = Obj n d Character M.empty []

----------------------------------------------------------------------
start = location "Road"
                 "You are on a twisty road."
                 [(E,end), (W,start)]
                 [ball, katydid]

end = location "Wall"
               "You are staring at a brick wall."
               [(W, start)]
               []

jail = location "Jail"
                "You are in jail."
                []
                []

ball = thingWithActions "ball"
                        "The inflated ball is brightly colored."
                        [ ("bounce", incScore 1 >> msg "Bouncy, bouncy!" >> stay)
                        , ("puncture", incScore (-10) >> go jail)
                        ]

katydid = character "katydid" "The katydid stares at you balefully."

----------------------------------------------------------------

