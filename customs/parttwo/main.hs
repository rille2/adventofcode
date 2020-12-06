import Data.List.Split
import Data.Set hiding (map, filter)
main = do
    p <- readFile "answers"
    print $ sum $ map sameAws $ map lines $ splitOn "\n\n" p
        where sameAws list = sum $ map fromEnum [((length . filter (==c)) (concat list)) == (length list) | c <- toList (fromList (concat list))]
