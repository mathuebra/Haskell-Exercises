trocar :: Int -> [Int]
trocar valor = reverter (calcularCedula valor [100, 50, 20, 10, 5, 1])
  where
    calcularCedula _ [] = []
    calcularCedula valor (c:cs)
      | qtdCedulas > 0 = repetirCedula c qtdCedulas ++ calcularCedula resto (c:cs)
      | otherwise = calcularCedula valor cs
      where
        qtdCedulas = valor `div` c
        resto = valor `mod` c
        repetirCedula _ 0 = []
        repetirCedula c n = c : repetirCedula c (n - 1)

reverter :: [Int] -> [Int]
reverter [] = []
reverter (a:b) = reverter b ++ [a]