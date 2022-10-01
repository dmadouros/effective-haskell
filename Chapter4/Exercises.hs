module Exercises where

  data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)

  showStringTree :: BinaryTree String -> String
  showStringTree Leaf = ""
  showStringTree (Branch left value right) = value <> " (" <> (showStringTree left) <> ") " <> "(" <> (showStringTree right) <> ")"

  showIntTree :: BinaryTree Int -> String
  showIntTree Leaf = ""
  showIntTree (Branch left value right) = (show value) <> " (" <> (showIntTree left) <> ") " <> "(" <> (showIntTree right) <> ")"

  addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
  addElementToIntTree Leaf newValue = Branch Leaf newValue Leaf
  addElementToIntTree (Branch left value right) newValue =
    if newValue <= value
      then (Branch (addElementToIntTree left newValue) value right)
      else  (Branch left value (addElementToIntTree right newValue))

  doesIntExist :: BinaryTree Int -> Int -> Bool
  doesIntExist (Branch Leaf value Leaf) query = query == value
  doesIntExist (Branch left value Leaf) query = 
    if query == value
      then True
      else doesIntExist left query
  doesIntExist (Branch Leaf value right) query = 
    if query == value
      then True
      else doesIntExist right query
  doesIntExist (Branch left value right) query = 
    if query == value
      then True
      else
        if query < value
          then doesIntExist left query
          else doesIntExist right query
