module Exercises where

reverse' xs = 
  foldl (\memo x -> x:memo) [] xs

reverse'' xs =
  foldr (\x memo -> memo <> [x]) [] xs

zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' fn (x:xs) (y:ys) =
  (fn x y):(zipWith' fn xs ys)

concatMap' fn =
  foldl (\memo x -> memo <> [(fn x)]) []
concatMap'' fn =
  foldr (\x memo -> (fn x):memo) []

f1 f g = foldr g 0 . map f
f2 f g = foldr (g . f) 0

-- f1 (+2) (+) [1, 2, 3]
-- foldr (+) 0 $ [3, 4, 5]

-- f2 (+2) (+) [1, 2, 3]
-- foldr ((+) . (+2)) 0 $ [1, 2, 3]