module Main where

import Game.HAdventure.Builder

train = location "Train" "You are on a train.  It is finally moving towards Washington, DC."
  !!! [ S --> snackCar ]
  @@@ [ joyia, wilson ]

joyia = character "Joyia" "It's a Joyia!  She is very beautiful."
  !!! [ "talk" --> msg "Joyia says hi." ]

wilson = character "Wilson" "Wilson is a cute, fuzzy brown bear."
  !+ takeable
  !!! [ "hug" --> msg "awwww."
      , "talk" --> msg "Wilson waves at you."
      ]

-- XXX This obviously doesn't work all that well.  Need to be able
-- to specify an object that doesn't exist yet, and match on string
-- rather than actual object identity.  Also need to be able
-- to change properties/flags of objects on the fly.
snackCar = location "Snack car" "You are in the snack car.  They sell snacks here."
  !!! [ N     --> train
      , "buy" --> do dirObjIs snacks ?: "You can't buy that."
                     has dollar ?: "You don't have any money."
                     msg "Here's your snack!  That will be two dollars."
                     destroy dollar >> destroy dollar
                     reveal snacks
      ]
  @@@ [ snacks ]

snacks = thing "snack" "It's a... you're actually not quite sure what it is."
  !!! [ "eat" --> do has snacks ?: "You don't have any snacks."
                     msg "You eat the snack.  It tasted... ok."
                     destroy snacks
                     incScore 10
      ]
  /// [ "snacks" ]
  !+ hidden

dollar = thing "dollar" "A normal, green, dollar bill.  One of the corners is a little bent, so you'll never be able to use it in a vending machine."

main = play cfg
  where cfg = mkConfig [] False (replicate 5 dollar) train