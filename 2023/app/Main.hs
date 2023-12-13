import Data.Char (isDigit)

main :: IO ()
main = do
  contents <- readFile "input/day1.txt"
  print . findPairs . lines $ contents

findPairs :: [String] -> Int
findPairs strs =
  sum $ map firstLast strs

firstLast :: String -> Int
firstLast str =
  findMatches str []

findMatches :: String -> String -> Int
findMatches [] numbers = do
  read [head numbers, last numbers]
findMatches ('o':'n':'e':tl) numbers = findMatches ("e" ++ tl) (numbers ++ "1")
findMatches ('t':'w':'o':tl) numbers = findMatches ("o" ++ tl) (numbers ++ "2")
findMatches ('t':'h':'r':'e':'e':tl) numbers = findMatches ("e" ++ tl) (numbers ++ "3")
findMatches ('f':'o':'u':'r':tl) numbers = findMatches ("r" ++ tl) (numbers ++ "4")
findMatches ('f':'i':'v':'e':tl) numbers = findMatches ("e" ++ tl) (numbers ++ "5")
findMatches ('s':'i':'x':tl) numbers = findMatches ("x" ++ tl) (numbers ++ "6")
findMatches ('s':'e':'v':'e':'n':tl) numbers = findMatches ("n" ++ tl) (numbers ++ "7")
findMatches ('e':'i':'g':'h':'t':tl) numbers = findMatches ("t" ++ tl) (numbers ++ "8")
findMatches ('n':'i':'n':'e':tl) numbers = findMatches ("e" ++ tl) (numbers ++ "9")
findMatches ('z':'e':'r':'o':tl) numbers = findMatches ("o" ++ tl) (numbers ++ "0")
findMatches str numbers
  | isDigit h = findMatches t (numbers ++ [h])
  | otherwise = findMatches t numbers
  where (h:t) = str
  
