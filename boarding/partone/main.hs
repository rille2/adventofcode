readP :: IO [(String, String)]
readP = do
    p <- readFile "passes"
    return [(take 7 ps, drop 7 ps) | ps <- lines p]

decode :: String -> Char -> Char -> Int -> Int -> Int
decode (x:[]) cl ch lo hi
  | x == ch = hi
  | otherwise = lo

decode (x:xs) cl ch lo hi
  | x == ch = decode xs cl ch (mid + 1) hi
  | x == cl = decode xs cl ch lo mid
    where mid = (lo + hi) `div` 2

decodeRow :: String -> Int
decodeRow row = decode row 'F' 'B' 0 127

decodeSeat :: String -> Int
decodeSeat seat = decode seat 'L' 'R' 0 7

seatId :: (String, String) -> Int
seatId (row, seat) = decodeRow row * 8 + decodeSeat seat

main :: IO ()
main = do
    p <- readP
    print $ maximum $ map seatId p
