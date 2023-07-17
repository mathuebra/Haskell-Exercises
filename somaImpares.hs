somaImpares :: [Int] -> Int
somaImpares lista
    | lista == [] = 0
    | otherwise = soma (selecionaImpares lista)

    where
    soma :: [Int] -> Int
    soma [] = 0
    soma (a:b) = a + soma b

    selecionaImpares :: [Int] -> [Int]
    selecionaImpares [] = []
    selecionaImpares (x:y)
        | x `mod` 2 == 1 = x : selecionaImpares y
        | otherwise = selecionaImpares y
