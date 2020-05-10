data Piece = One | Two | Empty
    deriving (Show, Eq)
data Table = Table [Piece] [Piece] [Piece]
    deriving (Show, Eq)

-- Ex 1 - Sa se verifice daca tabla este valida (cele 3 liste au lungimea 8 si fiecare jucator a pus pe tabla maxim 9 piese)
checkLungimeTabla :: Table -> Bool
checkLungimeTabla (Table l1 l2 l3) = length l1 == length l2 && length l2 == length l3 && length l3 == 8

checkNumarMaximMiscari :: Table -> Bool
checkNumarMaximMiscari (Table l1 l2 l3) = length f1 <= 9 && length f2 <= 9
    where 
        lista = l1 ++ l2 ++ l3
        -- [1,2,3] = filter (\x -> x `mod` 2 == 0) [2]
        f1 = filter (\x -> x == One) lista
        f2 = filter (\x -> x == Two) lista

checkTablaValida :: Table -> Bool
checkTablaValida tabla = checkLungimeTabla tabla && checkNumarMaximMiscari tabla

-- Table [One, Two, One, Two, One, One, One, Empty] 
--       [Empty, Empty, Empty, One, Two, One, Two, One] 
--       [Two, Two, One, Two, Two, Empty, Empty, Empty]

-- Ex 2 - pozitie valida 
--        1 <= x <= 3 && 0 <= y <= 7
data Position = P (Int, Int)

checkPozitiiConectate :: Position -> Position -> Bool
checkPozitiiConectate  (P(yP1, xP1)) (P(yP2, xP2))
    | (yP1 == yP2) = (xP1 == xP2 - 1) || (xP1 == xP2 + 1) || (xP1 == 0 && xP2 == 7) || (xP1 == 7 && xP2 == 0)
    | (yP1 /= yP2 && xP1 == xP2 && yP1 `mod` 2 /= 0) = (yP1 == yP2 + 1 || yP1 + 1 == yP2) 
    | otherwise = False

getLinie :: Int -> Table -> [Piece]
getLinie n (Table l1 l2 l3)
    | n == 1 = l1
    | n == 2 = l2
    | n == 3 = l3

checkPozitieGoala :: Position -> Table -> Bool
checkPozitieGoala (P(y, x)) table = linie!!x == Empty
    where linie = getLinie y table

move :: Table -> Position -> Position -> Maybe Table
move tabla pozitieStart pozitieFinal
    | (checkPozitiiConectate pozitieStart pozitieFinal == True) && (checkPozitieGoala pozitieStart tabla == False) && (checkPozitieGoala pozitieFinal tabla) == True = Just tabla
    | otherwise = Nothing

-- Ex 3
data EitherWriter a = EW {getValue :: Either String (a, String)}
