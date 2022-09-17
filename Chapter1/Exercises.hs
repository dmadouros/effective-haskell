module Exercises where

factorial num =
  let 
    factorial' num result =
      if num == 1
      then result
      else factorial' (num - 1) (num * result)
  in factorial' num 1

fibonacci num =
  let
    fibonacci' num idx a b = 
      if idx == num
      then
        a
      else
        fibonacci' num (idx + 1) b (a + b)
  in fibonacci' num 0 0 1

uncurriedAddition nums =
  let
    a = fst nums
    b = snd nums
  in a + b

myCurry fn = (\a b -> fn (a, b))
addition = myCurry uncurriedAddition

addOne = addition 1
addTwo = addition 2

myUncurry fn = (\(a, b) -> fn a b)
uncurriedAddition' = myUncurry (+)
