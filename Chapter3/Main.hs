module Main where

  pi :: Float
  pi = 3.14

  one, two :: Int
  one = 1
  two = 2

  three :: Int
  three = 3

  four :: Int
  four = 4

  calculateTotalCost :: Int -> Int
  calculateTotalCost basePrice =
    let
      priceWithServiceFee :: Int
      priceWithServiceFee = basePrice + 1
      customaryTip = 7 :: Int
    in priceWithServiceFee + customaryTip

--               :: x   -> (y   -> (z   -> Int))          
  addThreeNumbers:: Int -> (Int -> (Int -> Int))
  addThreeNumbers x y z =
    let
--      :: a   -> (g)
      f :: Int -> (Int -> (Int -> Int))
      f a =
        let
--          :: b   -> (h)
          g :: Int -> (Int -> Int)
          g b =
            let
--              :: c   -> (a + b + c)
              h :: Int -> Int
              h c = a + b + c
            in h
        in g
    in f x y z

  addOne :: Int -> Int
  addOne = (+ 1)

  incrementAndShow :: Int -> (Int -> String) -> String
  incrementAndShow num formatter = formatter (num + 1)

  incrementAndShow' :: Int -> (Int -> (Int -> String) -> String) -> String
  incrementAndShow' num f = f (num + 1) show
    
  main = print "hi"