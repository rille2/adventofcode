import Data.Char
import Data.List

countLetters :: String -> Char -> Int
countLetters str c = length $ filter (== c) str

minCount :: String -> Int
minCount str = length $ splitAt 2 str

isValidLine :: String -> Bool
isValidLine string = undefined

main = do
    s <- readFile "passwords"
    putStrLn $ show $ minCount $ (lines s) !! 5

    
