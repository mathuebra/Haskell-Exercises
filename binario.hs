binario :: Int -> [Int]

binario n
    | n == 0 = [0]
    | otherwise = binarioFinal n []
    where 
        binarioFinal 0 lista = lista
        binarioFinal m lista = binarioFinal (m `div` 2) ((m `mod` 2):lista)