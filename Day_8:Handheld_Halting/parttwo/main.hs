parse :: [String] -> [(String, Int)]
parse [] = []
parse (c:cs) = (take 3 c, val $ drop 4 c) : parse cs 
    where val str 
            | head str == '+' = read $ drop 1 str
            | otherwise = read str


allparse :: [(String, Int)] -> Int -> [[(String, Int)]]
allparse parsed ind
  | ind >= length parsed = []
  | fst p == "jmp" || fst p == "nop" = ((take (ind) parsed) ++ (opposite (fst p), snd p):(drop (ind + 1) parsed)) : (allparse parsed (ind + 1))
  | otherwise = allparse parsed (ind + 1)
        where p = parsed !! ind

opposite :: String -> String
opposite "jmp" = "nop"
opposite "nop" = "jmp"


main :: IO ()
main = do
    p <- readFile "ops"
    print $ head $ concat [traverseOps (head x) x [] 0 0 | x <- (allparse (parse (lines p)) 0)] 

traverseOps :: (String, Int) -> [(String, Int)] -> [Int] -> Int -> Int -> [Int]
traverseOps op allops traversed index accm
  | fst op == "nop" && ((index + 1) `elem` traversed) = []
  | fst op == "acc" && ((index + 1) `elem` traversed) = []
  | fst op == "jmp" && ((index + snd op) `elem` traversed) = []
  | fst op == "nop" && (index + 1) >= length allops = [accm]
  | fst op == "acc" && (index + 1) >= length allops = [accm + snd op]
  | fst op == "jmp" && (index + snd op) >= length allops = [accm]
  | fst op == "nop" = traverseOps (allops !! (index + 1)) allops (index:traversed) (index + 1) accm
  | fst op == "acc" = traverseOps (allops !! (index + 1)) allops (index:traversed) (index + 1) (accm + snd op)
  | fst op == "jmp" = traverseOps (allops !! (index + snd op)) allops (index:traversed) (index + snd op) accm
