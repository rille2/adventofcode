parse :: [String] -> [(String, Int)]
parse [] = []
parse (c:cs) = (take 3 c, val $ drop 4 c) : parse cs 
    where val str 
            | head str == '+' = read $ drop 1 str
            | otherwise = read str


main :: IO ()
main = do
    p <- readFile "ops"
    print $ traverseOps (head $ parse $ lines p) (parse $ lines p) [] 0 0

traverseOps :: (String, Int) -> [(String, Int)] -> [Int] -> Int -> Int -> Int
traverseOps op allops traversed index accm
  | fst op == "nop" && ((index + 1) `elem` traversed) = accm
  | fst op == "acc" && ((index + 1) `elem` traversed) = accm + snd op
  | fst op == "jmp" && ((index + snd op) `elem` traversed) = accm
  | fst op == "nop" = traverseOps (allops !! (index + 1)) allops (index:traversed) (index + 1) accm
  | fst op == "acc" = traverseOps (allops !! (index + 1)) allops (index:traversed) (index + 1) accm + snd op
  | fst op == "jmp" = traverseOps (allops !! (index + snd op)) allops (index:traversed) (index + snd op) accm
