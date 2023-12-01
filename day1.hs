import Data.Maybe (catMaybes)

parseStringToInt :: Char -> Maybe Int
parseStringToInt c 
    | c `elem` ['0'..'9'] = Just (read [c] :: Int)
    | otherwise = Nothing

parseLine :: String -> [Maybe Int]
parseLine strs = map parseStringToInt strs

filterJustIntegers :: [Maybe Int] -> [Int]
filterJustIntegers maybeInts = catMaybes maybeInts

sumFirstLast :: [Int] -> Int
sumFirstLast [] = 0
sumFirstLast [x] = read (show x ++ show x)
sumFirstLast xs = read (show (head xs) ++ show (last xs))

parseLines :: String -> Int
parseLines inputLines = (sumFirstLast . filterJustIntegers . parseLine) inputLines

main :: IO ()
main = do
  contentLines <- readFile "input1.txt"
  print $ foldr (\c acc -> c + acc) 0 (map parseLines $ lines contentLines)
