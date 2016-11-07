import Data.Char
import Data.List
import Test.QuickCheck

-- 1  Folosind recursivitate: f :: [Int] -> Int afiseaza produsul dintre diferenta dintre oricare doua elemente consecutive
-- f [3, 1, 4, 2, 5] = (3-1) * (1-4) * (4-2) * (2-5) = 36

prob1Rec :: [Int] -> Int
prob1Rec [] = 1
prob1Rec xs | length xs > 1 = (head xs - (head (tail xs))) * prob1Rec (tail xs)
            | otherwise = 1


-- 2 List comprehension: g :: [Int] -> String => semnul fiecarui numar intreg din intervalul [-9, 9], ignorand numerele din afara intervalului
-- g [5, 10, -5, 0] = "+-0"

prob2 :: [Int] -> String
prob2 xs = ['+' | x <- xs, x>0, x >= (-9), x <= 9] ++
           ['-' | x <- xs, x<0, x >= (-9), x <= 9] ++
           ['0' | x <- xs, x==0, x >= (-9), x <= 9]


-- 3 Folosing map, filter si fold: h :: [String] -> Int sterge toate vocalele din cuvintele care au lungime para si intoarce numarul total de consoane ramase
-- *Test> h ["ana", "are", "mere", "si", "pere"] = m r s p r = 5

prob3 :: [String] -> Int
prob3 xs = length $ elimina_dubluri (foldr (++) [] (map elimina_vocale xs))

lungime_para :: String -> Bool
lungime_para xs = if mod (length xs) 2 == 0 then True else False

rmChar ::  Char -> String -> String
rmChar ch = filter (/=ch)

rmCharsFold :: String -> String -> String
rmCharsFold = flip $ foldr rmChar

elimina_vocale :: String -> String
elimina_vocale str = rmCharsFold "aeiou" str

contain :: Char -> String -> Bool
contain _ [] = False
contain ch (x:xs) = if ch == x then True else contain ch xs

elimina_dubluri :: String -> String
elimina_dubluri [] = []
elimina_dubluri (x:xs) = if contain x xs then elimina_dubluri xs else x:elimina_dubluri xs


-- 4. Pentru un numar intreg x sa se determine lista de factori primi
--factor 315 = [3, 3, 5, 7]
