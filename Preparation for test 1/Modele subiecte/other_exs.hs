import Data.Char
import Data.List
import Test.QuickCheck

-- 1) Cauta ultimul element dintr-o lista
myLast :: [a] -> a
myLast [] = error "Lista este goala"
myLast [a] = a
myLast (x:xs) = myLast xs

-- 2) Cauta penultimul element dintr-o lista
myLastButOne :: [a] -> a
myLastButOne [] = error "Lista este goala"
myLastButOne [a,b] = a
myLastButOne (x:xs) = myLastButOne xs

-- 3) Cauta elementul al K-lea intr-o lista
findK :: [a] -> Int -> a
findK [] _ = error "Lista este goala"
findK xs k = if length xs > k-1 && k > 0
              then xs !! (k - 1)
              else error "Dimensiune prea mare sau negativa"

-- 4) Determina lungimea unei liste
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 5) Verifica daca este palindrom
isPalindrome :: (Eq a) =>[a] -> Bool
isPalindrome xs = xs == (reverse xs)
