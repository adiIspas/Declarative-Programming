import Test.QuickCheck

-- 1) Ridica la patrat fiecare element din lista
ridica_la_patrat :: [Int] -> [Int]
ridica_la_patrat xs = [x * x | x <- xs]

ridica_la_patrat_recursiv :: [Int] -> [Int]
ridica_la_patrat_recursiv [] = []
ridica_la_patrat_recursiv (x:xs) = x * x : ridica_la_patrat_recursiv xs

-- rulam quickCheck verifica_ridica_la_patrat pentru a testa functiile
verifica_ridica_la_patrat :: [Int] -> Bool
verifica_ridica_la_patrat xs = ridica_la_patrat xs == ridica_la_patrat_recursiv xs


-- 2) Selecteaza doar elementele impare din lista
elemente_impare :: [Int] -> [Int]
elemente_impare xs = [x | x <- xs, odd x]

elemente_impare_recursiv :: [Int] -> [Int]
elemente_impare_recursiv [] = []
elemente_impare_recursiv (x:xs) | odd x = x : elemente_impare_recursiv xs
                                | otherwise = elemente_impare_recursiv xs

verifica_elemente_impare :: [Int] -> Bool
verifica_elemente_impare xs = elemente_impare xs == elemente_impare_recursiv xs


-- 3) Calculeaza suma elementelor din lista
suma_elemente_recursiv :: [Int] -> Int
suma_elemente_recursiv [] = 0
suma_elemente_recursiv (x:xs) = x + suma_elemente_recursiv xs


-- 4) Calculeaza produsul elementelor din lista
produs_elemente_recursiv :: [Int] -> Int
produs_elemente_recursiv [] = 1
produs_elemente_recursiv (x:xs) = x * produs_elemente_recursiv xs


-- 5) Calculeaza suma patratelor elementelor impare
suma_patrate_elemente_impare :: [Int] -> Int
-- o alta varianta | suma_patrate_elemente_impare xs = suma_elemente_recursiv (ridica_la_patrat_recursiv (elemente_impare xs))
suma_patrate_elemente_impare xs = suma_elemente_recursiv [x * x | x <- xs, odd x]

suma_patrate_elemente_impare_recursiv :: [Int] -> Int
suma_patrate_elemente_impare_recursiv [] = 0
suma_patrate_elemente_impare_recursiv (x:xs) | odd x = x * x + suma_patrate_elemente_impare_recursiv xs
                                             | otherwise = suma_patrate_elemente_impare_recursiv xs

verifica_suma_patrate_elemente_impare :: [Int] -> Bool
verifica_suma_patrate_elemente_impare xs = suma_patrate_elemente_impare xs == suma_patrate_elemente_impare_recursiv xs
