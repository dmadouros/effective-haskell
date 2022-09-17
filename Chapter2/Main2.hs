module Main2 where

main = print "hi"

customGreeting "George" = "Oh, hey George!"
customGreeting name = "Hello, " <> name

matchNumber 0 = "zero"
matchNumber n = show n

matchList [1, 2, 3] = "one, two, three"
matchList list = show list

matchTuple ("hello", "world") = "greetings"
matchTuple tuple = show tuple

matchBool True = "yep"
matchBool bool = "this must be false"

radsToDegrees :: Float -> Int
radsToDegrees radians =
  let degrees = cycle [0..359]
      converted = truncate $ (radians * 360) / (2 * pi)
  in degrees !! converted

epicCycle inputList =
  cycleHelper inputList
  where
    cycleHelper [] = epicCycle inputList
    cycleHelper (x:xs) = x : cycleHelper xs

moreEpicCycle inputList =
  inputList <> moreEpicCycle inputList