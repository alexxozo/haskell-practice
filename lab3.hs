prim :: Int -> Bool
prim x = length [divizor | divizor <- [2..x `div` 2], x `mod` divizor == 0] == 0

-- exercitii map
firstEl :: [(Char, Int)] -> [Char]
firstEl lista = map (\x -> fst x) lista

sumList :: [[Int]] -> [Int]
sumList lista = map (\l -> sum l) lista

prel2 :: [Int] -> [Int]
prel2 lista = map (\x -> if x `mod` 2 == 0 then x `div` 2 else x * 2) lista

-- exercitii filter si map
findChar :: Char -> [String] -> [String]
findChar x lista = filter (\sir -> x `elem` sir) lista

numaiVocale :: [String] -> [String]
numaiVocale lista = map (\x -> filter (\y -> y `elem` ['a', 'e', 'i', 'o', 'u']) x) lista

-- ordonate
ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [_] = True
ordonataNat (h:t) = and $ [x < y | (x, y) <- zip (h:t) (t)]