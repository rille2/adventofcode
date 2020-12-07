import Data.Char
import Data.List

countLetters :: (Eq a) => [a] -> a -> Int
countLetters str c = length $ filter (== c) str

splitInTwo :: String -> (Int, Int)
splitInTwo string = splitInTwo' string 0

splitInTwo' :: String -> Int -> (Int, Int)
splitInTwo' string i
  | string !! i == '-' = ((read $ take i string), (read $ drop (i + 1) string))
  | otherwise = splitInTwo' string (i + 1)

getCharFromLine :: String -> Char
getCharFromLine string = head $ words string !! 1

getWordFromLine :: String -> String
getWordFromLine string = words string !! 2

isValidLine :: String -> Bool
isValidLine line = lo <= ct && ct <= hi
    where (lo, hi) = splitInTwo $ words line !! 0
          c = getCharFromLine line
          w = getWordFromLine line
          ct = countLetters w c

checkAllLines :: [String] -> Int
checkAllLines cs = checkAllLines' cs 0

checkAllLines' :: [String] -> Int -> Int
checkAllLines' [] n = n
checkAllLines' (c:cs) n
  | isValidLine c = checkAllLines' cs (n + 1)
  | otherwise     = checkAllLines' cs n


main = do
    s <- readFile "passwords"
    let lines' = lines s 
    putStrLn $ show $ checkAllLines lines'
    
