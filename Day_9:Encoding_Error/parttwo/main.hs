import Prelude hiding (sequence)

sequence :: [Int] -> Int -> [Int]
sequence xs x = take x xs

search :: [Int] -> Int -> Int -> Int -> [(Int, Int)] -> Int
search xs lo hi x tried
  | (lo, hi) `elem` tried = -1
  | sum seq == x = minimum seq + maximum seq
  | sum seq > x = search xs lo mid x ((lo, hi):(take 1 tried))
  | sum seq < x = search xs mid hi x ((lo, hi):(take 1 tried))
    where mid = (lo + hi) `div` 2
          seq = sequence xs mid
 
find :: [Int] -> Int -> Int
find (x:xs) y
  | s == -1 = find xs y
  | otherwise = s
    where s = search (x:xs) 0 (length xs) y []

main = do
    f <- readFile "input"
    print $ find [read x | x <- lines f] 3199139634
