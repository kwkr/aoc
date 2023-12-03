import Data.List.Split (splitOn)
import Data.Char (isSpace)


trim:: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

extractGames::String -> [[String]]
extractGames str = foldr (\x acc -> acc ++ x) [] $ map s $ splitOn ";" $ last $ splitOn ":" str

s::String->[[String]]
s str = map words $ map trim $ splitOn "," str

matchGame::String -> String
matchGame str = str

extractIds:: String -> Int
extractIds str =  read $ last $ words $ head $ splitOn ":" str

isValid::[[String]] -> Bool
isValid i = foldr (\(n:r) acc -> case (head r) of
    "red" -> (read n) <= 12 && acc
    "green" -> (read n) <= 13 && acc
    "blue" -> (read n) <= 14 && acc 
    _ -> False) True i

-- limits for the values
-- 12 red, 13 green, 14 blue 

main :: IO ()
main = do
  contentLines <- readFile "input2.txt"
  let games = lines contentLines
  let ids = map extractIds games
  let g = map extractGames games
  let validRows = map isValid g
  let input = zip ids validRows
  print $ foldr (\(id,valid) acc -> case valid of
      True -> acc + id
      False -> acc) 0 input

