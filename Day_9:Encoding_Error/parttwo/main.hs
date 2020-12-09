findSeq :: [Int] -> Int -> Int -> Int
findSeq (x:xs) currsl sc
  | sum seq == sc = minimum seq + maximum seq
  | currsl == length xs = findSeq xs 1 sc
  | otherwise = findSeq (x:xs) (currsl + 1) sc
    where seq = (x : (take currsl xs))

main = do
    f <- readFile "input"
    print $ findSeq (read x | x <- lines s) 1 3199139634