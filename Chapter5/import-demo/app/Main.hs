module Main where
  import Data.Char qualified as Char (isPrint)
  import Data.Text hiding            (length, filter)
  import Data.Text qualified as T    (length, filter)
  import Data.Text.Encoding  as T    (decodeUtf8, encodeUtf8)

  countNonPrintableCharacters :: String -> Int
  countNonPrintableCharacters =
    Prelude.length . Prelude.filter (not . Char.isPrint)

  countNonPrintableCharactersInText :: Text -> Int
  countNonPrintableCharactersInText =
    T.length . T.filter (not . Char.isPrint) . T.decodeUtf8 . T.encodeUtf8

  countNonPrintableCharactersStringAndText :: String -> (Int, Int)
  countNonPrintableCharactersStringAndText input =
    (countNonPrintableCharacters input, countNonPrintableCharactersInText $ pack input)

  main :: IO()
  main =
    print $ countNonPrintableCharacters "\v\t\aHello\r\n"