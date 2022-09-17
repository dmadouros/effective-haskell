module Main where
-- makeGreeting salutation person = 
--   let messageWithTrailingSpace = salutation <> " "
--   in messageWithTrailingSpace <> person

-- extendedGreeting person =
--   let joinWithNewLines a b = a <> "\n" <> b
--       helloAndGoodbye hello goodbye =
--         let hello' = makeGreeting hello person
--             goodbye' = makeGreeting goodbye person
--         in joinWithNewLines hello' goodbye'
--   in helloAndGoodbye "Hello" "Goodbye"

-- letWhereGreeting name place =
--   let
--     salutation = "Hello " <> name
--     meetingInfo = location "Tuesday"
--   in salutation <> " " <> meetingInfo
--   where
--     location day = "we met at " <> place <> " on a " <> day

-- main = print $ makeGreeting "Hello" "George"

printSmallNumber num =
  if num < 10
  then print num
  else print "the number is too big!"

sizeNumber num =
  if num < 3
  then "that's a small number"
  else
    if num < 10
    then "that's a medium sized number"
    else "that's a big number"

guardSize num
  | num < 3 = "that's a small number"
  | num < 10 = "that's a medium number"
  | num < 100 = "that's a pretty big number"
  | num < 1000 = "wow, that's a giant number"
  | otherwise = "that's an unfathomably big number"

main = printSmallNumber 3
