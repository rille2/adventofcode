quicksort :: ( Ord a ) => [ a ] -> [ a ]
quicksort [] = []
quicksort ( x : xs ) =
    let smallerSorted = quicksort [ a | a <- xs , a <= x ] 
        biggerSorted = quicksort [ a | a <- xs , a > x ]
        in smallerSorted ++ [ x ] ++ biggerSorted

add (a, b) (x, y) = (a + x, b + y)
mul (a, b) = a * b

diffs :: [Int] -> (Int, Int)
diffs (x:[]) = (1, 1)
diffs (x:xs)
  | head xs - x == 2 = add (1, 0) $ diffs xs
  | head xs - x == 3 = add (0, 1) $ diffs xs
  | otherwise = diffs xs

main :: IO ()
main = do
    f <- readFile "input"
    print $ diffs $ quicksort [read x | x <- lines f]
