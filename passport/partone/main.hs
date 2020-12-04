import Data.List.Split
import Data.Char

getPassports :: IO [[(String, String)]]
getPassports = do
    p <- readFile "false"
    return $ map prune $ map words (splitOn "\n\n" p)
        where prune [] = []
              prune (c:cs) = (splut !! 0, splut !! 1) : prune cs
                  where splut = splitOn ":" c

-- reverse $ take 2 $ reverse string cm in
-- reverse $ drop 2 $ reverse strign num

validPair :: (String, String) -> Bool
validPair ("byr", yr) = length yr == 4 && ryr >= 1920 && ryr <= 2002 
    where ryr = read yr

validPair ("iyr", isy) = length isy == 4 && risy >= 2010 && risy <= 2020 
    where risy = read isy

validPair ("eyr", ex) = length ex == 4 && rex >= 2020 && rex <= 2030 
    where rex = read ex

validPair ("hgt", hg) = (afx == "in" && lng >= 59 && lng <= 76) || (afx == "cm" && lng >= 150 && lng <= 193)
    where afx = reverse $ take 2 $ reverse hg 
          lng = read $ reverse $ drop 2 $ reverse hg

validPair ("hcl", hc) = head hc == '#' && length hc == 7 && and [isNumber c || isLetter c | c <- drop 1 hc]

validPair ("ecl", ec) = sum (map toInt [ec == s | s <- ["amb", "blu", "brn","gry", "grn", "hzl", "oth"]]) == 1

validPair ("pid", p) = length p == 9 && and [isNumber x | x <- p]

validPair ("cid", _ ) = True

toInt :: Bool -> Int
toInt True = 1
toInt False = 0

validPassPort :: [(String, String)] -> Bool
validPassPort [] = True
validPassPort (c:cs) = validPair c && validPassPort cs

allValid :: [[(String, String)]] -> Int
allValid [] = 0
allValid (c:cs) = toInt (validPassPort c) + allValid cs

main :: IO ()
main = do
    p <- getPassports
    print $  allValid p