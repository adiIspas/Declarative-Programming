-- Adrian Ispas, 343

-- Functiile care pot fi folosite sunt listate la sfarsitul fisierului
-- categoria A - functii de baza
-- categoria B - functii din biblioteci (fara map, filter, fold)
-- categoria C - map, filter, fold

-- Redenumiti fisierul de test in GrupaNume.hs 
-- exemplu: 343ISPAS.hs

-- la final, pentru a trimite testul 
-- incarcati fisierul pe linkul acesta: https://sendtomycloud.com/pdtest7

-- 1.  Sa se calculeze cel mai mare divizor comun al 2 numere
--
--     Puteti folosi doar recursie si functii din categoria A
gcdRec :: Int -> Int -> Int
gcdRec 0 x = x
gcdRec x 0 = x	
gcdRec x y = if x > y then gcd (x - y) y else gcd x (y - x)


-- 2. Pentru un nr intreg sa se afiseze lista de divizori ai numarului.
--
--     Puteti folosi doar descrieri de liste (list comprehension) si 
--     functii din categoriile A si B

divs :: Int -> [Int]
divs n = [ x | x <- [1..n], n `mod` x == 0 ]


-- 3. Pentru o lista de numere, sa se calculeze cel mai mare divizor comun al elementelor pozitive.
-- (11 simboluri)
--     Puteti folosi doar functii din categoriile A, B, C si functiile definite mai sus
--     (fara recursie si descrieri de liste)

gcdList :: [Int] -> Int
gcdList = foldr gcd 0 . filter (>0) 

-- 4.  Sa se implementeze metoda de sortare QuickSort.

-- Quicksort imparte lista de sortat in doua subliste mai usor de sortat. 
-- Pașii algoritmului sunt:
--    Se alege un element al listei - pivot (poate fi primul element al listei)
--    Se reordoneaza lista astfel incat toate elementele mai mici decat pivotul sa fie plasate inaintea pivotului si toate elementele mai mari sa fie dupa pivot. Dupa aceasta partitionare, pivotul se afla in pozitia sa finala.
--    Se sorteaza sublista de elemente mai mici decat pivotul și sublista de elemente mai mari decat pivotul. 

--     Puteti folosi functii din categoriile A, B, C, recursie sau list comprehension


qsort :: [Int] -> [Int]
qsort [] = []
qsort [x] = [x]
qsort (x:n) = qsort (filter (<x) n) ++ [x] ++ qsort (filter (>x) n) 

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
ord :: Char -> Int
chr :: Int -> Char
Intervale
[first..], [first,second..], [first..last], [first,second..last]
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
takeWhile isLower "goodBye" = "good"       

take :: Int -> [a] -> [a]
take 4 "goodbye" = "good"

dropWhile :: (a->Bool) -> [a] -> [a]       
dropWhile isLower "goodBye" = "Bye"        

drop :: Int -> [a] -> [a]
drop 4 "goodbye" = "bye"

elem :: (Eq a) => a -> [a] -> Bool         
elem 'd' "goodbye" = True                  

replicate :: Int -> a -> [a]
replicate 5 '*' = "*****"

zip :: [a] -> [b] -> [(a,b)]
zip [1,2,3,4] [1,4,9] = [(1,1),(2,4),(3,9)
-}

{- Categoria C. Map, Filter, Fold
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
