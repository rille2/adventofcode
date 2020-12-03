isTree :: Int -> String -> Int
isTree x line 
  | line !! (x `mod` (length line)) == '#' = 1
  | otherwise = 0

allTrees :: Int -> [String] -> (Int, Int) -> Int
allTrees x lins (stepx, stepy)
  | drop (stepy) lins == [] = 0
  | otherwise = isTree (x + stepx) (lins !! (stepy)) + allTrees (x + stepx) (drop stepy lins) (stepx, stepy)

main = do
     s <- readFile "trees"
     putStrLn $ show $ product [allTrees 0 (lines s) x | x <- [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]]
