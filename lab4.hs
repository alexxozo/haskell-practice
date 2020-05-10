-- Produs
produsRec :: [Int] -> Int
produsRec [] = 1
produsRec (h:t) = h * produsRec t

produsFold :: [Int] -> Int
produsFold lista = foldr (*) 1 lista

-- Verificare true
andRec :: [Bool] -> Bool
andRec [] = True
andRec (h:t) = h && andRec t

andRecFold :: [Bool] -> Bool
andRecFold lista = foldr (&&) True lista

-- Concatenare lista de liste
concatRec :: [[Int]] -> [Int]
concatRec [] = []
concatRec [[]] = []
concatRec (h:t) = h ++ concatRec t

concatFold :: [[Int]] -> [Int]
concatFold lista = foldr (++) [] lista

-- Remove chars
rmChar :: Char -> String -> String
rmChar litera string = filter (\x -> x /= litera) string