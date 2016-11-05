-- Declarative Programming
-- Lab 2
--

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [ div x 2 | x <- xs, even x]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) | even x = div x 2 : halveEvensRec xs
                     | otherwise = halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x >= lo, x <= hi]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ [] = []
inRangeRec lo hi (x:xs) | x >= lo, x <= hi = x : inRangeRec lo hi xs
                        | otherwise = inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRange lo hi xs == inRangeRec lo hi xs



-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = length [x | x <- xs, x > 0]

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs) | x > 0 = 1 + countPositivesRec xs
                         | otherwise = countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives xs == countPositivesRec xs



-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount xs = round (0.9 * (fromIntegral xs))

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = sum [discount x | x <- xs, discount x <= 19900]

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (x:xs) | discount x <= 19900 = discount x + pennypincherRec xs
                       | otherwise = pennypincherRec xs

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs == pennypincherRec xs



-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits xs = product [digitToInt x | x <- xs, isDigit x]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) | isDigit x = digitToInt x * multDigitsRec xs
                     | otherwise = multDigitsRec xs

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs == multDigitsRec xs



-- 6. capitalise

lowerRec :: String -> String
lowerRec [] = ""
lowerRec (x:xs) = toLower x : lowerRec xs

lower :: String -> String
lower [] = ""
lower xs = [toLower x | x <- xs]

-- List-comprehension version
capitalise :: String -> String
capitalise [] = ""
capitalise (x:xs) = toUpper x : [toLower x | x <- xs]

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec [] = ""
capitaliseRec (x:xs) = toUpper x : lowerRec xs

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise xs = capitalise xs == capitaliseRec xs


-- 7. title

-- List-comprehension version
title :: [String] -> [String]
title [] = []
title (x:xs) = capitalise x : [if length x >= 4 then capitalise x else lower x | x <- xs]

-- Recursive version
titleRec :: [String] -> [String]
titleRec [] = []
titleRec (x:xs) = capitaliseRec x : titleRec_2 xs

titleRec_2 :: [String] -> [String]
titleRec_2 [] = []
titleRec_2 (x:xs) | length x >= 4 = capitaliseRec x : titleRec_2 xs
                  | otherwise = lowerRec x : titleRec_2 xs

-- mutual test
prop_title :: [String] -> Bool
prop_title xs = title xs == titleRec xs


-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind ch pos len xs = [x | x <- xs, x !! pos == ch, length x == len, pos < length x]

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec _ _ _ [] = []
crosswordFindRec ch pos len (x:xs) | pos < length x, x !! pos == ch, length x == len = x : crosswordFindRec ch pos len xs
                                   | otherwise = crosswordFindRec ch pos len xs

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind ch pos len xs = crosswordFind ch pos len xs == crosswordFindRec ch pos len xs



-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str ch = [i | i <- [0..length str-1], str !! i == ch]

-- Recursive version
searchRec :: String -> Char -> [Int]
searchRec [] _ = []
searchRec str ch = searchPositionRec str ch 0

searchPositionRec :: String -> Char -> Int -> [Int]
searchPositionRec [] _ _ = []
searchPositionRec str ch i | i < length str, str !! i == ch = i : searchPositionRec str ch (i + 1)
                           | otherwise = if i < length str then searchPositionRec str ch (i + 1) else []


-- Mutual test
prop_search :: String -> Char -> Bool
prop_search str ch = search str ch == searchRec str ch


-- 10. contains

suffixes :: String -> [String]
suffixes xs = [drop i xs | i <- [0..length xs]]

-- List-comprehension version
contains :: String -> String -> Bool
contains str_1 str_2 = [] /= [ True | s <- suffixes str_1, isPrefixOf str_2 s ]

-- Recursive version
containsRec :: String -> String -> Bool
containsRec _ [] = True
containsRec [] str = False
containsRec str_1 str_2 = isPrefixOf str_2 str_1 || containsRec (tail str_1) str_2


-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains str_1 str_2 = contains str_1 str_2 == containsRec str_1 str_2
