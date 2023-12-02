import Data.Maybe (catMaybes)
import Data.List (tails, isPrefixOf)

getElements :: String -> [String]
getElements str = filter (\x -> (length x > 0)) (tails str)

filterJustIntegers :: [Maybe Int] -> [Int]
filterJustIntegers maybeInts = catMaybes maybeInts

sumFirstLast :: [Int] -> Int
sumFirstLast [] = 0
sumFirstLast [x] = read (show x ++ show x)
sumFirstLast xs = read (show (head xs) ++ show (last xs))

mapInt:: String -> Maybe Int
mapInt str
    | head str `elem` ['0'..'9'] = Just (read [head str] :: Int)
    | "one" `isPrefixOf`  str = Just 1 
    | "two" `isPrefixOf`  str = Just 2
    | "three" `isPrefixOf`  str = Just 3
    | "four" `isPrefixOf`  str = Just 4
    | "five" `isPrefixOf`  str = Just 5
    | "six" `isPrefixOf`  str = Just 6
    | "seven" `isPrefixOf`  str = Just 7
    | "eight" `isPrefixOf`  str = Just 8
    | "nine" `isPrefixOf`  str = Just 9
    | "zero" `isPrefixOf`  str = Just 0
    | otherwise = Nothing

mapStrings::[String]->[Int]
mapStrings strs = filterJustIntegers (map mapInt strs)

main :: IO ()
main = do
  contentLines <- readFile "input1.txt"
  let numbers = map sumFirstLast $ map mapStrings (map getElements (lines contentLines))
  print $ foldr (\x acc -> x + acc) 0 numbers
