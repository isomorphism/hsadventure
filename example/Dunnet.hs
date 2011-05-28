{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main where

import Game.HAdventure.Builder

dead_end = location "Dead end" (
  "You are at a dead end of a dirt road.  The road goes to the " ++
  "east. In the distance you can see that it will eventually fork off. " ++
  "The trees here are very tall royal palms, and they are spaced " ++
  "equidistant from each other." )
  @@@ [shovel, trees]
  !!! [ E --> e_w_road
      ]

trees = thing "trees"
  "They are palm trees with a bountiful supply of coconuts in them."
  !!! [ "climb" --> msg $ "You manage to get about two feet up the tree " ++
                          "and fall back down.  You notice that the tree " ++
                          "is very unsteady."

      , "shake" --> do msg $ "You begin to shake a tree, and notice a " ++
                             "coconut begin to fall from the air. As " ++
                             "you try to get your hand up to block it, " ++
                             "you feel the impact as it lands on your " ++
                             "head."
                       die
      ]
  /// ["tree"]
  !+ hidden
  !- takeable

shovel = thing "shovel"
  "It is a normal shovel with a price tag attached that says $19.99."

e_w_road = location "E/W Dirt road" (
  "You are on the continuation of a dirt road.  There are more trees " ++
  "on both sides of you.  The road continues to the east and " ++
  "west. There is a large boulder here." )
   @@@ [boulder, trees]
   !!! [ E --> fork
       , W --> dead_end
       ]

boulder = thing "large boulder"
  "It is just a boulder.  It cannot be moved."
  /// ["boulder"]
  !- takeable
  !+ hidden

fork = location "Fork" (
  "You are at a fork of two passages, one to the northeast, and one to " ++
  "the southeast.  The ground here seems very soft. You can also go " ++
  "back west." )
   !!! [ W --> e_w_road
       , SE --> se_nw_road
       , NE --> ne_sw_road
       , "dig" --> do has shovel ?: noShovelMsg
                      msg "I think you found something."
                      reveal cpu
       ]

noShovelMsg = "You have nothing with which to dig."

cpu = thing "CPU card" (
  "The CPU board has a VAX chip on it.  It seems to have 2 Megabytes " ++
  "of RAM onboard." )
  /// ["cpu", "card", "board", "chip"]
  !!! [ words "put insert install" -->
          do indirObjIs computer ?: "You can't put that there."
             msg $ "As you put the CPU board in the computer, it " ++
                   "immediately springs to life.  The lights start " ++
                   "flashing, and the fans seem to startup."
             destroy cpu
      ]

se_nw_road = location "SE/NW road"
  "You are on a southeast/northwest road."
  @@@ [food]
  !!! [ NW --> fork
      , SE --> bear_hangout
      ]

food = thing "food"
  "It looks like some kind of meat.  Smells pretty bad."
  !!! [ "eat" --> do msg "That tasted horrible."
                     destroy food
      ]

bear_hangout = location "Bear hangout" (
  "You are standing at the end of a road.  A passage leads back to the " ++
  "northwest." )
  !!! [ SE --> do require (isHere bear)
                  msg $ "The bear is very annoyed that you would be " ++
                        "so presumptuous as to try and walk right " ++
                        "by it.  He tells you so by tearing your " ++
                        "head off."
                  die
      , NW --> se_nw_road
      , SW --> hidden_area
      ]
  @@@ [ bear ]

bear = character "ferocious bear"
  "It looks like a grizzly to me."
  /// ["bear"]

key = thing "shiny brass key" "It's shiny."
  /// ["key"]

hidden_area = location "Hidden area" (
  "You are in a well-hidden area off to the side of a road.  Back to " ++
  "the northeast through the brush you can see the bear hangout." )
  !!! [ NE --> bear_hangout
      ]
  @@@ [bracelet]

bracelet = thing "emerald bracelet" "It's made with emeralds.  Duh."
  /// ["bracelet"]

ne_sw_road = location "NE/SW road" "You are on a northeast/southwest road."
  !!! [ SW --> fork
      , NE --> building
      ]

building = location "Building front" (
  "You are at the end of the road.  There is a building in front of you " ++
  "to the northeast, and the road leads back to the southwest." )
  !!! [ SW -->  ne_sw_road
      , NE -->  do has key ?: "You don't have a key that can open this door."
                   go hallway
      ]

hallway = location "Old Building hallway" (
  "You are in the hallway of an old building.  There are rooms to the " ++
  "east and west, and doors leading out to the north and south." )
  !!! [ S --> building
      , E --> mail_room
      , W --> computer_room
      , N --> msg "You don't have a key that can open this door."
      ]

mail_room = location "Mailroom" (
  "You are in a mailroom.  There are many bins where the mail is " ++
  "usually kept.  The exit is to the west." )
  !!! [ W --> hallway ]

computer_room = location "Computer room" (
  "You are in a computer room.  It seems like most of the equipment has " ++
  "been removed.  There is a VAX 11/780 in front of you, however, with " ++
  "one of the cabinets wide open.  A sign on the front of the machine " ++
  "says: This VAX is named 'pokey'.  To type on the console, use the " ++
  "'type' command.  The exit is to the east." )
  !!! [ E --> hallway
      ]
  @@@ [computer]

-- This is what state is needed for.  The computer is either on or off,
--   which affects what you can do and also the description.
computer = thing "computer" ""
  !- takeable
  !+ hidden

globalActions =
  [ "feed" --> do dirObjIs bear ?: "You can't feed that."
                  has food      ?: "You have nothing with which to feed it."
                  msg $ "The bear takes the food and runs away " ++
                        "with it. He left something behind."
                  destroy food
                  destroy bear
                  reveal key
  , "eat" --> msg "Why would you want to eat THAT?"
  , "dig" --> do has shovel ?: noShovelMsg
                 msg "Digging here reveals nothing."
  ]

main = play conf
  where conf = mkConfig globalActions False [] dead_end

