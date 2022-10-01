module Calculator where
  import Text.Read (readEither)

  data Expr = 
    Lit Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    deriving (Show)

  eval :: Expr -> Int
  eval expr =
    case expr of
      Lit num -> num
      Add arg1 arg2 -> eval' (+) arg1 arg2
      Sub arg1 arg2 -> eval' (-) arg1 arg2
      Mul arg1 arg2 -> eval' (*) arg1 arg2
      Div arg1 arg2 -> eval' div arg1 arg2
      where
        eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Int
        eval' operator arg1 arg2 =
          operator (eval arg1) (eval arg2)

  safeEval :: Expr -> Either String Int
  safeEval expr =
    case expr of
      Lit num -> Right num
      Add arg1 arg2 -> eval' (+) arg1 arg2
      Sub arg1 arg2 -> eval' (-) arg1 arg2
      Mul arg1 arg2 -> eval' (*) arg1 arg2
      Div arg1 arg2 -> eval' div arg1 arg2
      where
        eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Either String Int
        eval' operator arg1 (Lit 0) = Left "Error: division by zero"
        eval' operator arg1 arg2 =
          Right $ operator (eval arg1) (eval arg2)

  parse :: String -> Either String Expr
  parse str =
    case parse' (words str) of
      Left err        -> Left err
      Right (e, [])   -> Right e
      Right (_, rest) -> Left $ "Found extra tokens: " <> (unwords rest)
      where
        parse' :: [String] -> Either String (Expr, [String])
        parse' [] = Left "unexpected end of expression"
        parse' (token:rest) =
          case token of
            "+" -> parseBinary Add rest
            "*" -> parseBinary Mul rest
            "-" -> parseBinary Sub rest
            "/" -> parseBinary Div rest
            lit ->
              case readEither lit of
                Left err -> Left err
                Right lit' -> Right (Lit lit', rest)
        parseBinary :: (Expr -> Expr -> Expr) -> [String] -> Either String (Expr, [String])
        parseBinary exprConstructor args =
          case parse' args of
            Left err -> Left err
            Right (firstArg, rest) ->
              case parse' rest of
                Left err -> Left err
                Right (secondArg, rest'') ->
                  Right $ (exprConstructor firstArg secondArg, rest'')
  run :: String -> String
  run str =
    case parse str of
      Left err -> "Error: " <> err
      Right expr -> 
        let answer = show $ eval expr
        in "The answer is: " <> answer

  prettyPrint :: Expr -> String
  prettyPrint expr = (prettyPrint' 0 expr) <> " = " <> (show $ eval expr)
  prettyPrint' :: Int -> Expr -> String
  prettyPrint' depth expr =
    case expr of
      (Lit num) -> show num
      (Add arg1 arg2) -> prettyPrint'' depth "+" arg1 arg2
      (Mul arg1 arg2) -> prettyPrint'' depth "*" arg1 arg2
      (Div arg1 arg2) -> prettyPrint'' depth "/" arg1 arg2
      (Sub arg1 arg2) -> prettyPrint'' depth "-" arg1 arg2
    where
      prettyPrint'' :: Int -> String -> Expr -> Expr -> String
      prettyPrint'' depth operator arg1 arg2 =
        if depth == 0
         then (prettyPrint' (depth + 1) arg1) <> " " <> operator <> " " <> (prettyPrint' (depth + 1) arg2)
         else "( " <> (prettyPrint' (depth + 1) arg1) <> " " <> operator <> " " <> (prettyPrint' (depth + 1) arg2) <> " )"
