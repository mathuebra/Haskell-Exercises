primo :: Int -> Bool
primo 0 = False
primo n
    | n <= 1 = False
    | possuiDivisor n 2 = False
    | otherwise = True
    where
        possuiDivisor :: Int -> Int -> Bool
        possuiDivisor n x
            | x * x > n = False
            | n `mod` x == 0 = True
            | otherwise = possuiDivisor n (x+1)
