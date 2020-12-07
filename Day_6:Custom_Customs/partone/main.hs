import Data.List.Split
import Data.Set hiding (map)
main = do
    p <- readFile "answers"
    print $ sum $ map size $ map fromList [ concat $ lines line | line <- splitOn "\n\n" p]
