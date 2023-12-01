import Data.Maybe (catMaybes)

main :: IO ()
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

main = do
  content <- readFile "input1.txt"
  let linesRead = lines content
  let result = map parseLine linesRead
  let filtered = map filterJustIntegers result
  let r = map sumFirstLast filtered
  let x = foldr (\c acc -> c + acc) 0 r
  print x
