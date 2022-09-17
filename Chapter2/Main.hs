module Main where

countdown n =
  if n <= 0 then []
  else n : countdown (n - 1)

factors num =
  factors' num 2
  where
    factors' num fact
      | num == 1 = []
      | (num `rem` fact) == 0 = fact : factors' (num `div` fact) fact
      | otherwise = factors' num (fact + 1)

isBalanced s =
  0 == isBalanced' 0 s
  where
    isBalanced' count s
      | null s = count
      | head s == '(' = isBalanced' (count + 1) (tail s)
      | head s == ')' = isBalanced' (count - 1) (tail s)
      | otherwise = isBalanced' count (tail s)

doubleElems :: [Int] -> [Int]
-- doubleElems nums = 
--   if null nums
--   then []
--   else
--     let 
--       hd = head nums
--       tl = tail nums
--     in (2 * hd) : doubleElems tl
doubleElems = foldr doubleElem []
  where
    doubleElem num lst = (2 * num) : lst
doubleElems' elems = foldr (applyElem (*2)) [] elems
  where
    applyElem f elem accumulator = f elem : accumulator
map' f = foldr (applyElem f) []
  where
    applyElem f elem accumulator = (f elem) : accumulator
doubleWithMap elems = map' (*2) elems
map'' f xs =
  if null xs then []
  else f (head xs) : map'' f (tail xs)

checkGuestList guestList name =
  name `elem` guestList

foodCosts =
  [("Ren", 10.00)
  ,("George", 4.00)
  ,("Porter", 27.50)
  ]

partyBudget isAttending =
  foldr (+) 0 . map snd .filter (isAttending . fst)

pairs as bs =
  let as' = filter (`elem` bs) as
      bs' = filter odd bs
      mkPairs a = map (\b -> (a, b)) bs'
  in concat $ map mkPairs as'
pairs' as bs =
  [(a, b) | a <- as, b <- bs, a `elem` bs, odd b]

partyBudget' isAttending willEat foodCost guests =
  foldl (+) 0 $
  [ foodCost food
  | guest <- map fst guests
  , food <- map snd guests
  , willEat guest food
  , isAttending guest
  ]

pairwiseSum xs ys =
  let sumElems pairs =
        let a = fst pairs
            b = snd pairs
        in a + b
  in map sumElems $ zip xs ys
pairwiseSum' xs ys = map (uncurry (+)) $ zip xs ys

main = putStrLn "nothing to see here"
