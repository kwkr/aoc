import Control.Monad (when)
import Data.Char (isSpace)
import Data.List
import Data.List.Split (splitOn)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

extractGames :: String -> [[String]]
extractGames str = foldr ((\x acc -> acc ++ x) . splitElements) [] (splitOn ";" $ last $ splitOn ":" str)

splitElements :: String -> [[String]]
splitElements str = map (words . trim) (splitOn "," str)

matchGame :: String -> String
matchGame str = str

extractIds :: String -> Int
extractIds str = read $ last $ words $ head $ splitOn ":" str

-- limits for the values
-- 12 red, 13 green, 14 blue

filterSortColor :: String -> [[String]] -> Int
filterSortColor k = read . head . head . take 1 . reverse . sortBy (\(a : _) (b : _) -> compare (read a :: Int) (read b :: Int)) . filter (\x -> last x == k)

main :: IO ()
main = do
  contentLines <- readFile "input2.txt"
  let games = lines contentLines
  let ids = map extractIds games
  let g = map extractGames games
  -- g is a list of lists of lists containing all pairs val,color like ["12","red"]
  let redSortedGames = map (filterSortColor "red") g
  let greenSortedGames = map (filterSortColor "green") g
  let blueSortedGames = map (filterSortColor "blue") g
  let result = zip3 redSortedGames greenSortedGames blueSortedGames
  print $
    foldr
      (\(a, b, c) acc -> a * b * c + acc)
      0
      result
