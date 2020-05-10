import Data.Char

-- Fibonacci
fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2 = n
    | otherwise = fibonacciCazuri (n-1) + fibonacciCazuri (n-2)

-- Recursie peste liste
semiPareRecDestr :: [Int] -> [Int]
semiPareRecDestr l
    | null l = l
    | even h = h `div` 2 : t'
    | otherwise = t'
    where
        h = head l
        t = tail l
        t' = semiPareRecDestr t

semiPareRecEq :: [Int] -> [Int]
semiPareRecEq [] = []
semiPareRecEq (h:t)
    | even h = h `div` 2 : t'
    | otherwise = t'
    where t' = semiPareRecEq t

-- List comprehension
semiPareComp :: [Int] -> [Int]
semiPareComp l = [x `div` 2 | x <- l, even x]

-- inInterval
inInterval :: Int -> Int -> [Int] -> [Int]
inInterval a b lista
    | null lista = lista
    | x >= a && x <= b = x : t'
    | otherwise = t'
    where
        x = head lista
        t = tail lista
        t' = inInterval a b t 

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec a b [] = []
inIntervalRec a b (h:t)
    | (h >= a && h <= b) = h : t'
    | otherwise = t'
    where t' = inIntervalRec a b t

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b lista = [x | x <- lista, x >= a && x <= b]

-- Numaram pozitive
pozitive :: [Int] -> Int
pozitive [] = 0
pozitive (h:t)
    | h > 0 = 1 + t'
    | otherwise = t'
    where t' = pozitive t

pozitiveComp :: [Int] -> Int
pozitiveComp lista = foldl (+) 0 $ map (\x -> if x > 0 then 1 else 0) lista

-- Lista pozitiilor impare
pozitiiImpareHelper :: [Int] -> Int -> [Int]
pozitiiImpareHelper [] _ = []
pozitiiImpareHelper (h:t) pozCurenta
    | h `mod` 2 /= 0 = pozCurenta : pozitiiImpareHelper t (pozCurenta-1)
    | otherwise = pozitiiImpareHelper t (pozCurenta-1)

pozitiiImpare :: [Int] -> [Int]
pozitiiImpare [] = []
pozitiiImpare lista = pozitiiImpareHelper lista (length (lista))

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp lista = [snd x | x <- zip lista [0..length lista], fst x `mod` 2 /= 0]

-- Produsul cifrelor din string
multDigits :: String -> Int
multDigits "" = 1
multDigits (h:t)
    | isDigit h = digitToInt h * multDigits t
    | otherwise = multDigits t

multDigitsComp :: String -> Int
multDigitsComp string = foldl (*) 1 [digitToInt x | x <- string, isDigit x == True]

-- Discounturi
discount :: [Float] -> [Float]
discount [] = []
discount (h:t)
    | rez < 200 = rez : t
    | otherwise = discount t
    where rez = (h - 0.25 * h)

discountComp :: [Float] -> [Float]
discountComp lista = [rez | x <- lista, let rez = x - (0.25 * x), rez < 200]