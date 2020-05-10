data Linie = L[Int]
    deriving Show
data Matrice = M[Linie]

-- Ex 1 - intoarce toate liniile de lungime n
-- liniiN (M[L[1,2,3],L[4,5],L[2,3,6,8],L[8,5,3]]) 3
-- [L [1,2,3],L [8,5,3]]
liniiN :: Matrice -> Int -> [Linie]
liniiN (M lista) n = filter (\(L linie) -> length linie == n) lista

-- Ex 2 - verifica daca toate liniile de lungime n au numai elemente pozitive
-- doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3
-- True
-- doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3
-- False
doarPozN :: Matrice -> Int -> Bool
doarPozN matrice n = all (\(L linie) -> foldr (*) 1 linie > 0) (liniiN matrice n)

-- Ex 3 - verifica suma elementelor de pe fiecare linie e egala cu n
-- verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10
-- False
-- verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25
-- True
verifica :: Matrice -> Int -> Bool
verifica (M lista) n = all (\(L linie) -> sum linie == n) lista

-- Ex 4 - o instanta a clasei show pentru matrice
showLinie :: Linie -> String
showLinie (L []) = "\n"
showLinie (L (h:t)) = show h ++ " " ++ showLinie (L t) 

showMatrice :: Matrice -> String
showMatrice (M []) = ""
showMatrice (M (h:t)) = showLinie h ++ showMatrice (M t) 

linii :: Matrice -> [Linie]
linii (M x) = x

showMatriceConcat :: Matrice -> String
showMatriceConcat matrice = concat([showLinie(linie) | linie <- linii(matrice)])

instance Show Matrice where
    show = showMatrice
