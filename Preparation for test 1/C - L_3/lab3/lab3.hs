-- Declarative Programming
-- Lab 3
--

import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate n str = if n < 0 || n > length str then error "Numarul nu corespunde cu lungimea sirului"
               else drop n str ++ take n str

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else
                                if k < 0 then (-1) * k `mod` l
                                else k `mod` l

-- 3.
makeKey :: Int -> [(Char, Char)]
makeKey n = zip ['A'..'Z'] (rotate n ['A'..'Z'])

-- 4.

lookUp :: Char -> [(Char, Char)] -> Char
lookUp ch xs = head ([y | (x,y) <- xs, x == ch] ++ [ch])

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec ch [] = ch
lookUpRec ch ((x,y):xs) | x == ch = y
                        | otherwise = if length xs == 0
                                      then ch else lookUpRec ch xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp ch xs = lookUp ch xs == lookUpRec ch xs

-- 5.
encipher :: Int -> Char -> Char
encipher n ch = lookUp ch (makeKey n)

-- 6.
normalize :: String -> String
normalize str = [toUpper s | s <- str, isAlpha s || isDigit s]

-- 7.
encipherStr :: Int -> String -> String
encipherStr n str = [encipher n s | s <- normalize str]

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey key = [(y,x) | (x,y) <- key]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((x,y):key) = (y,x) : reverseKeyRec key

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey key = reverseKey key == reverseKeyRec key

-- 9.
decipher :: Int -> Char -> Char
decipher n ch = lookUp ch (reverseKey (makeKey n))

decipherStr :: Int -> String -> String
decipherStr n str = [decipher n s | s <- normalize str]

-- 10.
contains :: String -> String -> Bool
contains [] _ = False
contains _ [] = True
contains str substr = isPrefixOf substr str || contains (tail str) substr

-- 11.
candidates :: String -> [(Int, String)]
candidates str = [(i, decipherStr i str) | i <- [0..25], candidate i str]

candidate :: Int -> String -> Bool
candidate n str = contains (decipherStr n str) "THE" || contains (decipherStr n str) "AND"



-- Optional Material

-- 12.
splitEachFive :: String -> [String]
splitEachFive str | length str > 5 = take 5 str : splitEachFive (drop 5 str)
                  | otherwise = [fill str]

fill :: String -> String
fill str = str ++ replicate (5 - length str) 'X'



-- 13.
prop_transpose :: String -> Bool
prop_transpose = undefined

-- 14.
encrypt :: Int -> String -> String
encrypt = undefined

-- 15.
decrypt :: Int -> String -> String
decrypt = undefined

-- Challenge (Optional)

-- 16.
countFreqs :: String -> [(Char, Int)]
countFreqs = undefined

-- 17
freqDecipher :: String -> [String]
freqDecipher = undefined
