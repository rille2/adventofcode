import Data.List.Split
import Data.Set hiding (map, filter)
main = do
    p <- readFile "answers"
    print $ sum $ map (\x -> sum $ map fromEnum [((length . filter (==c)) (concat x)) == (length x) | c <- toList $ fromList $ concat x]) $ map lines $ splitOn "\n\n" p
