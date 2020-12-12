import Data.Set hiding (drop, take, filter)

quicksort :: ( Ord a ) => [ a ] -> [ a ]
quicksort [] = []
quicksort ( x : xs ) =
    let smallerSorted = quicksort [ a | a <- xs , a <= x ] 
        biggerSorted = quicksort [ a | a <- xs , a > x ]
        in smallerSorted ++ [ x ] ++ biggerSorted

add (a, b) (x, y) = (a + x, b + y)
mul (a, b) = a * b

children :: [Int] -> Int -> [Int]
children all x = [n | n <- all, x - n == 3 || x - n == 1 || x - n == 2]

getNchildren :: [Int] -> Int -> Int
getNchildren all 0 = 1
getNchildren all n 
  | length c == 1 = getNchildren all (head c)
  | length c == 2 = 2 * getNchildren all (maximum c)
  | length c == 3 && (length $ children all (maximum c)) == 2 = 2 * getNchildren all (maximum c)
  | otherwise = sum [getNchildren all x | x <- c]
    where c = children all n
          
find :: [Int] -> Int
find all = find' all [0] (maximum all)

find' :: [Int] -> [Int] -> Int -> Int
find' all seq max
  | last seq == max = 1
  | otherwise = sum [find' (filter (> last seq + 1) all) (seq ++ [x]) max | x <- all,  diff seq x]

diff :: [Int] -> Int -> Bool
diff list x = d >= 1 && d <= 3 
    where d = (x - (last list))

main :: IO ()
main = do
    f <- readFile "input"
    let a = quicksort (0:[read x :: Int | x <- lines f])
    print $ getNchildren a (maximum a)
