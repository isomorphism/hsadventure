{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main where

import Game.HAdventure.Builder
import Game.HAdventure.Engine

import Control.Monad (msum)

tree = location "Tree" (
  "You are standing at the base of a large tree. " ++
  "To the east you can see a door." )
  !!! [ E --> doorloc ]

doorloc = location "Door"
  "You are in the middle of a large plain. To the west is a large tree."
  @@@ [door]
  !!! [ W --> tree ]

door = thing "door"
  "It is a plain wooden door, standing by itself in the middle of the plain."
  !!! [ "open" --> do notP (isHere troll) ==> trollAppears
                      require passTrollTest
                      destroy troll
                      go plains
      , "knock" --> msg "\"Who is it?\""
      ]

trollAppears = do msg "Suddenly, an ugly troll appears from behind the door!"
                  reveal troll

troll = character "troll"
  "It is an extremely ugly troll.  No, like, seriously ugly."

passTrollTest = do msg trollQuestion
                   c <- promptUser "What... is your favorite color?\""
                   let ok = (c == "blue")
                   msg $ if ok then "\"Right!\"" else "\"Nope!\""
                   return ok
  where
    trollQuestion = "\"Before you pass through this door, you " ++
                    "must answer my question!"

plains = location "The plains beyond" (
  "You are in the middle of a large plain.  Behind you to the west " ++
  "is an open door.  You have the feeling that you couldn't go back " ++
  "through it, even if you wanted to (which you don't, particularly)." )
  @@@ [ elephant ]

elephant = thing "elephant" "It is a large, grey elephant."

main = startAt tree