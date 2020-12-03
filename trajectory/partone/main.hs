readTrees :: FilePath -> IO [String]
readTrees path = do
    string <- readFile path
    return $ lines string

isTree :: Int -> String -> Int
isTree x line 
  | line !! (x `mod` (length line)) == '#' = 1
  | otherwise = 0

allTrees :: Int -> [String] -> Int
allTrees _ (l:[]) = 0
allTrees x (l:ls) = isTree (x + 3) (head ls) + allTrees (x + 3) ls

main :: IO ()
main = do
    trees <- readTrees "trees"
    putStrLn $ show $ allTrees 0 trees
        
