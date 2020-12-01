trips :: [Int] -> [(Int, Int, Int)]
trips list = [(x, y, z) | x <- list, y <- list, z <- list]

findTrip :: [(Int, Int, Int)] -> Int -> Maybe Int
findTrip [] _ = Nothing
findTrip ((x, y, z):xs) num 
    | (x + y + z) == num = (Just $ x * y * z)
    | otherwise          = findTrip xs num

findFromFile :: FilePath -> Int -> IO ()
findFromFile path num = do
    str <- readFile path
    let ints = [(read x) | x <- words str]
    putStrLn $ show $ findTrip (trips ints) num

main :: IO ()
main = findFromFile "report" 2020
