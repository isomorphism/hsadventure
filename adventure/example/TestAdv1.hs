{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main where

import Game.HAdventure.Builder

start = location "Road" (
  "You are on a twisty road. To the west, it sort of loops " ++
  "back around onto itself.  To the east, it continues on " ++
  "out of sight." )
  @@@ [ball]
  !!! [ E --> end
      , W --> start
      ]

end = location "Court"
  "You are on a twisty road. There is a four square court here."
  !!! [ "play" --> do has ball ?: "You don't have anything to play with."
                      incScore 10
                      msg fourSquareMsg
                      reveal katydid
      , W --> start
      ]
  @@@ [spoon, spoon]

fourSquareMsg = "You begin playing four square by yourself.  Suddenly, a "
  ++ "new friend appears and joins you!"

jail = location "Jail" "You are in jail.  Your cell is very boring, albeit acoustically interesting.  You need to do something to cheer yourself up, or you will go crazy."
  !!! [ "sing"   --> do msg singMsg
                        reveal key
      , "escape" --> do has key ?: "You cannot escape."
                        incScore 10
                        msg "You have escaped!"
                        go start
      ]

singMsg = "Your singing scares away a rat.  He appears to have dropped something."

key = thing "key" "It's one of those old-fashioned keys with a ring on one end and several metal flanges on the other."

ball = thing "ball" "The inflated ball is brightly colored."
  !!! [ "bounce"   --> incScore 1 >> msg "Bouncy, bouncy!"
      , "puncture" --> incScore (-10) >> go jail
      ]

katydid = character "katydid" "The katydid stares at you balefully."

spoon = thing "spoon" "The spoon is shiny and silver."

main = startAt start