parse :: String -> [Int]
parse s = [read x | x <- lines s]

valid :: Int -> [Int] -> Bool
valid int list = int `elem` [x + y | x <- list, y <- list, y /= x]

findInValid :: [Int] -> Int -> Int
findInValid list i
  | valid (list !! i) (drop (i - 25) (take i list)) = findInValid list (i + 1)
  | otherwise = list !! i

main = do
    f <- readFile "input"
    print $ findInValid ( parse f) 25
