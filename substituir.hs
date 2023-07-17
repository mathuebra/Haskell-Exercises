substituir :: Int -> Int -> [Int] -> [Int]
--substituir _ _ [] = []
--substituir x y (a:b) = if x == a then y : substituir x y b else a : substituir x y b
substituir _ _ [] = []
substituir x y (a:b)
    | x == a = y : substituir x y b
    | otherwise = a : substituir x y b