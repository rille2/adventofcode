import Data.List.Split
import Data.Char
import Data.Set hiding (filter, map)

parseBags :: [String] -> [(String, [String])]
parseBags [] = []
parseBags (c:cs) = (concat $ words $ init $ head splut, prune [tail $ concat $ init $ words $ tail w | w <- splitOn "," $ splut !! 1]) : parseBags cs
    where splut = splitOn "bags contain" c

prune :: [String] -> [String]
prune ("oother":_) = []
prune strings = strings

getChildren :: String -> [(String, [String])]-> [String]
getChildren [] _ = []
getChildren b bags = find b bags 
    where find b [] = []
          find b (c:cs) 
            | b == fst c = snd c
            | otherwise = find b cs

traverseBag :: (String, [String]) -> [String] -> [(String, [String])] -> [String]
traverseBag (cbag, bagchildren) traversed allbags
  | "shinygold" `elem` bagchildren = cbag : traversed
  | bagchildren == [] = []
  | otherwise = concat [traverseBag (b, getChildren b allbags) (cbag:traversed) allbags | b <- bagchildren]




main = do
    s <- readFile "bags"
    print $ length $ toList $ fromList $ concat [traverseBag b [] (parseBags $ lines s) | b <- (parseBags $ lines s)]
