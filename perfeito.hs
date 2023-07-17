perfeito :: Int -> Bool
perfeito n
    | n == 0 = False
    | n == 1 = True
    | otherwise = soma (fatores n) == n

    where
    soma :: [Int] -> Int
    soma [] = 0
    soma (a:b) = a + soma b
    
    fatores :: Int -> [Int]
    fatores 0 = []
    fatores n = [ i | i <- [1..n `div` 2], n `mod` i == 0 ]