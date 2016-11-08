import Data.Char
import Data.List
import Test.QuickCheck

-- 1) Verifica daca un numar este palindrom fara a folosi functii de librarie
-- ex palindrom 121 = True

palindrom :: Int -> Bool
palindrom n = n == (invers n)

invers :: Int -> Int
invers 0 = 0
invers n = (mod n 10) * (10^(lungime n - 1)) + (invers (div n 10))

lungime :: Int -> Int
lungime 0 = 0
lungime n = 1 + lungime (div n 10)

-- 2) Cate numere dintr-o lista sunt palindrom
-- ex palindroame [121,123,131] = 2

palindroame :: [Int] -> Int
palindroame xs = sum [1 | x <- xs, palindrom x]

-- 3) Mai multe cifre sau litere intr-un string?

cifreSiLitere :: String -> String
cifreSiLitere str = if (cifre str) > (litere str) then "Cifre" else "Litere"

cifre :: String -> Int
cifre [] = 0
cifre (x:xs) = if isDigit x then 1 + cifre xs else cifre xs

litere :: String -> Int
litere [] = 0
litere (x:xs) = if isAlpha x then 1 + litere xs else litere xs

-- 4) Verifica daca un numar este perfect. Suma divizorilor fara numar este egala cu numarul

perfect :: Int -> Bool
perfect n = (sum (divizori n) - n) == n

divizori :: Int -> [Int]
divizori n = filter (\x -> mod n x == 0) [1..n]

-- 5) Verifica daca pentru o lista data diferenta termenilor consecutivi din lista este numar perfect
-- [4-3, 9-4] ~> [1, 5] ~> 6

perfDiffs :: [Int] -> Bool
perfDiffs xs = perfect (foldr (+) 0 (diff xs))

diff :: [Int] -> [Int]
diff [] = []
diff xs | length xs > 1 = (-1) * (head xs - (head (tail xs))) : diff (tail xs)
        | otherwise = []

{- Categoria A. Functii de baza
div, mod :: Integral a => a -> a -> a
even, odd :: Integral a => a -> Bool
(+), (*), (-), (/) :: Num a => a -> a -> a
(<), (<=), (>), (>=) :: Ord => a -> a -> Bool
(==), (/=) :: Eq a => a -> a -> Bool
(&&), (||) :: Bool -> Bool -> Bool
not :: Bool -> Bool
max, min :: Ord a => a -> a -> a
isAlpha, isAlphaNum, isLower, isUpper, isDigit :: Char -> Bool
toLower, toUpper :: Char -> Char
digitToInt :: Char -> Int
ord :: Char -> Int
chr :: Int -> Char
-}


{- Categoria B. Functii din biblioteci
sum, product :: (Num a) => [a] -> a
sum [1.0,2.0,3.0] = 6.0
product [1,2,3,4] = 24


and, or :: [Bool] -> Bool
and [True,False,True] = False
or [True,False,True] = True


maximum, minimum :: (Ord a) => [a] -> a
maximum [3,1,4,2]  =  4
minimum [3,1,4,2]  =  1


reverse :: [a] -> [a]
reverse "goodbye" = "eybdoog"


concat :: [[a]] -> [a]
concat ["go","od","bye"]  =  "goodbye"


(++) :: [a] -> [a] -> [a]
"good" ++ "bye" = "goodbye"


(!!) :: [a] -> Int -> a
[9,7,5] !! 1  =  7


length :: [a] -> Int
length [9,7,5]  =  3


head :: [a] -> a
head "goodbye" = 'g'


tail :: [a] -> [a]
tail "goodbye" = "oodbye"


init :: [a] -> [a]
init "goodbye" = "goodby"


last :: [a] -> a
last "goodbye" = 'e'


takeWhile :: (a->Bool) -> [a] -> [a]
takeWhile isLower "goodDay" = "good"


take :: Int -> [a] -> [a]
take 4 "goodbye" = "good"


dropWhile :: (a->Bool) -> [a] -> [a]
dropWhile isLower "goodBye" = "Bye"


drop :: Int -> [a] -> [a]
drop 4 "goodbye" = "bye"


elem :: (Eq a) => a -> [a] -> Bool
elem 'd' "gdbye" = True


replicate :: Int -> a -> [a]
replicate 5 '*' = "*****"


zip :: [a] -> [b] -> [(a,b)]
zip [1,2,3,4] [1,4,9] = [(1,1),(2,4),(3,9)


-}


{- Catgoria C. Map, Filter, Fold
map :: (a -> b) -> [a] -> [b]
map (+3) [1,2] = [4,5]


filter :: (a -> Bool) -> [a] -> [a]
filter even [1,2,3,4] = [2,4]


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr max 0 [1,2,3,4] = 4


(.) :: (b -> c) -> (a -> b) -> a -> c
($) :: (a -> b) -> a -> b
(*2) . (+3) $ 7 = 20


flip :: (a -> b -> c) -> b -> a -> c
flip (-) 2 3 = 1
-}
