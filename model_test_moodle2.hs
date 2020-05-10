data Piesa = One | Two | Empty
    deriving (Show, Eq)

data Careu = C [Piesa]
    deriving (Show, Eq)

type Tabla = [Careu]

-- Ex 1 -- sa se verifica daca tabla este valida == 3 careuri de lungime 8 si fiecare jucator a pus maxim 9 piese

verificaCareu :: [Careu] -> Bool
verificaCareu lista = length lista' == 3 && sum lista' == 24
    where 
        lista' = map (\(C piese) -> length piese) lista

nrPiese :: Tabla -> Piesa -> Int
nrPiese lista piesaCautata = sum (map (\(C piese) -> length (filter (\x -> x == piesaCautata) piese)) lista)

validTabla :: Tabla -> Bool
validTabla lista = verificaCareu lista && nrPiese lista One <= 9 && nrPiese lista Two <= 9

table1 :: Tabla
table1 = [C [Empty, One, Two, One, Empty, Empty, Two, One], C [Two, Empty, One, Two, One, Two, One, Two], C [Two, Two, One, Empty, Empty, One, Two, One]]

table2 :: Tabla
table2 = [C [Two, One, Two, One, Empty, Empty, Two, One],
          C [Two, Empty, One, Two, One, Two, One, Two],
          C [Two, Two, One, Empty, Empty, One, Two, One]]

table3 :: Tabla
table3 = [C [Empty, One, Empty, Empty, Empty, Empty, Two, One],
          C [Two, Empty, One, Two, One, Two, One, Two],
          C [Two, Empty, One, Empty, Empty, One, Two, One]]

table4 :: Tabla
table4 = [C [Empty,Empty,Two,One,Empty,Empty,Two,One],
          C [Two,One,One,Two,One,Two,One,Two],
          C [Two,Two,One,Empty,Empty,One,Two,One]]
table5 :: Tabla
table5 = [C [Empty,Empty,Two,One,Empty,Empty,Two,One],
          C [Two,One,One,Two,One,Two,One,Two]]

test11 = validTabla table1 == True
test12 = validTabla table2 == False
test13 = validTabla table3 == True
test14 = validTabla table5 == False

-- Ex 2 -- move pozitie, pozitie valida == (0 <= y <= 2) and (0 <= x <= 7)
data Pozitie = Poz (Int,Int)

instance Show Pozitie where
  show (Poz (i,j)) = "careul " ++ show i ++ " pozitia " ++ show j

validPozitii :: Pozitie -> Pozitie -> Bool
validPozitii (Poz (yP1, xP1)) (Poz (yP2, xP2))
    | yP1 == yP2 = (xP1 == xP2 + 1) || (xP1 + 1 == xP2 + 1) || (xP1 == 0 && xP2 == 7) || (xP2 == 7 && xP1 == 0)
    | yP1 /= yP2 = (xP1 == xP2) && (xP1 `mod` 2 == 0) && (yP1 == yP2 + 1 || yP1 + 1 == yP2)

getPozitie :: Pozitie -> Tabla -> Piesa
getPozitie (Poz (yP1, xP1)) tabla = linie!!xP1
    where (C linie) = tabla!!yP1

interschimbareLinie :: Int -> Int -> Careu -> [Piesa]
interschimbareLinie poz1 poz2 (C lista) = start ++ [lista!!poz2] ++ mijloc ++ [lista!!poz1] ++ final
    where 
        start = take poz1 lista
        mijloc = take (poz2 - poz1 - 1) (drop (poz1 + 1) lista)
        final = drop (poz2 + 1) lista 

interschimbarePiese :: Pozitie -> Pozitie -> Tabla -> Tabla
interschimbarePiese (Poz (yP1, xP1)) (Poz (yP2, xP2)) tabla
    | yP1 == yP2 = take (yP1+1) tabla ++ [(C (interschimbareLinie xP1 xP2 (tabla!!yP1)))] ++ drop (2 - yP1) tabla

move :: Tabla -> Pozitie -> Pozitie -> Maybe Tabla
move tabla poz1 poz2 =
    if validPozitii poz1 poz2 && getPozitie poz1 tabla /= Empty && getPozitie poz2 tabla == Empty
        then Just tabla
        else Nothing

test21 = move table2 (Poz (0,2)) (Poz(1,2)) == Nothing
test22 = move table1 (Poz (0,2)) (Poz(1,2)) == Nothing
test23 = move table1 (Poz (0,1)) (Poz(1,1))
       == Just ([C [Empty,Empty,Two,One,Empty,Empty,Two,One],
                  C [Two,One,One,Two,One,Two,One,Two],
                  C [Two,Two,One,Empty,Empty,One,Two,One]])
                      -- table4
test24 = move table1 (Poz (2,1)) (Poz(1,1))
        == Just ([C [Empty,One,Two,One,Empty,Empty,Two,One],
                  C [Two,Two,One,Two,One,Two,One,Two],
                  C [Two,Empty,One,Empty,Empty,One,Two,One]])
