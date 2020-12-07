import Data.List.Split
import Data.Char
import Data.Set hiding (filter, map)

parseBags :: [String] -> [(String, [String])]
parseBags [] = []
parseBags (c:cs) = (concat $ words $ init $ head splut, prune [concat $ init $ words $ tail w | w <- splitOn "," $ splut !! 1]) : parseBags cs
    where splut = splitOn "bags contain" c

prune :: [String] -> [String]
prune ("noother":_) = []
prune strings = strings

convert :: [String] -> [String]
convert [] = []
convert (x:xs) = [tail x | _ <- [1..(digitToInt $ head x)]] ++ (convert xs)

getChildren :: String -> [(String, [String])]-> [String]
getChildren [] _ = []
getChildren b bags = find b bags 
    where find b [] = []
          find b (c:cs) 
            | b == fst c = snd c
            | otherwise = find b cs

val :: String -> Int
val [] = 0
val str = digitToInt $ head str

traverseCount :: String -> [(String, [String])] -> Int
traverseCount bag allbags 
  | getChildren bag allbags == [] = 0
  | otherwise = sum $ [val b | b <- getChildren bag allbags] ++ [traverseCount b allbags | b <- convert $ getChildren bag allbags]

main = do
    s <- readFile "bags"
    print $ traverseCount "shinygold" (parseBags $ lines s)
    -- print $ head $ parseBags $ lines s

