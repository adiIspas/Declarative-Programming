-- Declarative Programming
-- Lab 3
--


import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate n xs = if n <=0 || n > length xs then error "Numarul nu este mai mare decat 0 sau este mai mare decat lungimea listei"
              else drop n xs ++ take n xs

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3.
str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
makeKey :: Int -> [(Char, Char)]
makeKey n = zip str (rotate n str)

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp ch ((a,b):xs) | a == ch = b
                     | otherwise = if length xs == 0 then ch else lookUp ch xs

-- 5.
encipher :: Int -> Char -> Char
encipher n ch = lookUp ch (makeKey n)

-- 6.
normalize :: String -> String
normalize xs = [toUpper x | x <- xs, ord x /= ord ' ']

-- 7.
encipherStr :: Int -> String -> String
encipherStr n xs = [encipher n x | x <- normalize xs]

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [(b,a) | (a,b) <- xs]

-- 9.
decipher :: Int -> Char -> Char
decipher n ch = lookUp ch (reverseKey (makeKey n))

decipherStr :: Int -> String -> String
decipherStr n xs = [decipher n x | x <- xs]

-- 10.
prop_cipher :: Int -> String -> Bool
prop_cipher = undefined

-- 11.
contains :: String -> String -> Bool
contains (x:xs) str | isPrefixOf str xs = True
                    | otherwise = if length xs == 0 then False else contains xs str

-- 12.
candidates :: String -> [(Int, String)]
candidates xs = candidatesN xs 1

candidatesN :: String -> Int -> [(Int, String)]
candidatesN xs n | ((n < length xs) && (contains (dechipherStr n xs) "THE" || contains (dechipherStr n xs) "AND")) = (n, (dechipherStr n xs)) : candidatesN xs (n + 1)
                 | otherwise = if n < length xs then candidatesN xs (n + 1)
