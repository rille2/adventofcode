findInValid :: [Int] -> Int -> Int
findInValid list i
  | list !! i `elem` combs = findInValid list (i + 1)
  | otherwise = list !! i
      where combs = [x + y | x <- (drop (i - 25) (take i list)), y <- (drop (i - 25) (take i list)), y /= x] 
    

main = do 
    f <- readFile "input"
    print $ findInValid [read x | x <- lines f] 25
