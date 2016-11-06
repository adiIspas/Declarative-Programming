import Data.Char
import Data.List
import Test.QuickCheck

-- 1) Transforma un sir de caractere in lista codurilor ASCII
ords :: [Char] -> [Int]
ords xs = [ord x | x <- xs]

ordsRec :: [Char] -> [Int]
ordsRec [] = []
ordsRec (x:xs) = ord x : ordsRec xs

prop_ords :: [Char] -> Bool
prop_ords xs = ords xs == ordsRec xs

-- Solutia cu map
-- Functia map aplica functia ord fiecarui element din xs
ordsMap :: [Char] -> [Int]
ordsMap xs = map ord xs

-- 2) Selecteaza cifrele dintr-un sir de caractere
digits :: [Char] -> [Char]
digits xs = [x | x <- xs, isDigit x]

digitsRec :: [Char] -> [Char]
digitsRec [] = []
digitsRec (x:xs) | isDigit x = x : digitsRec xs
                 | otherwise = digitsRec xs

prop_digits :: [Char] -> Bool
prop_digits xs = digits xs == digitsRec xs

-- Solutia cu filter
-- Functia filter verifica compatibilitatea din xs elementelor cu ajutorul
-- functiei isDigit
digitsFilter :: [Char] -> [Char]
digitsFilter xs = filter isDigit xs

-- 3) Calculeaza suma elementelor dintr-o lista
-- Functia foldr (asociativa la dreapta) aplica functia de actualizare (+)
-- fiecarui element plecand de la o valoare initiala 0
suma :: [Int] -> Int
suma xs = foldr (+) 0 xs


-- 4) Calculeaza suma patratelor numerelor pozitive
-- folosind map, filter, fold
-- Filtream numerele pozitive, aplicam ridicarea la patrat pe fiecare element
-- pozitiv, iar la final aplicam functia de actualizare (+) peste toate patratele
s_p_n_p :: [Int] -> Int
s_p_n_p xs =  foldr (+) 0 (map (^2) (filter (>0) xs))


-- 5) Functii anonime
-- (\x -> x > 0) este o functie anonima ce selecteaza elementele pozitive
numere_pozitive :: [Int] -> [Int]
numere_pozitive xs = filter (\x -> x > 0) xs

-- 6) Sectiuni
-- (>0) este echivalentul functiei anonime de mai sus
numere_pozitive_2 :: [Int] -> [Int]
numere_pozitive_2 xs = filter (>0) xs

-- 7) Compunerea functiilor
-- Se face cu operatorul dot (.)
s_p_n_p_2 :: [Int] -> Int
s_p_n_p_2 xs = foldr (+) 0 . map (^2) . filter (>0) xs
