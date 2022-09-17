module Flatfile
  ( calcColumn
  ) where 
import Data.Char

calcColumn :: Int -> String
calcColumn = toChars . sanitize . toBase 26

toBase :: Int -> Int -> [Int]
toBase _ 0 = [0]
toBase b n = 
  toBase' [] n 
    where
      toBase' :: [Int] -> Int -> [Int]
      toBase' a 0 = a
      toBase' a n = toBase' (r:a) q 
        where (q, r) = n `divMod` b

sanitize :: [Int] -> [Int]
sanitize digits@[_] = digits
sanitize (0:rest)   = rest
sanitize (1:rest)   = 0:rest
sanitize digits     = digits

toChars :: [Int] -> [Char]
toChars = map chr . map (+(ord 'A'))
