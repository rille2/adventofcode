
getReport :: FilePath -> IO [Int]
getReport path = do
    str <- readFile path
    return $ toInts $ words str

toInts :: [String] -> [Int]
toInts []     = []
toInts (x:xs) = (read x) : (toInts xs)

pairs :: [Int] -> [(Int, Int)]
pairs list = [(x, y) | x <- list, y <- list]

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

mulPair :: (Int, Int) -> Int
mulPair (x, y) = x * y

findPair :: [(Int, Int)] -> Int -> Maybe Int
findPair [] _ = Nothing
findPair (x:xs) num 
    | sumPair x == num = (Just $ mulPair x)
    | otherwise        = findPair xs num


printFind :: Maybe Int -> IO ()
printFind (Just x) = putStrLn $ show x
printFind Nothing  = putStrLn "Could not find"

findFromFile :: FilePath -> Int -> IO ()
findFromFile path num = do
    ints <- getReport path
    printFind $ findPair (pairs ints) num
    
main :: IO ()
main = findFromFile "report" 2020

