-- Declarative Programming
-- Lab 2
--

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [div x 2 | x <- xs, mod x 2 == 0]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) | mod x 2 == 0 = div x 2 : halveEvensRec xs
                     | otherwise = halveEvensRec xs
-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs


-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x >= lo, x <= hi ]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi [] = []
inRangeRec lo hi (x:xs) | x >= lo, x <= hi = x : inRangeRec lo hi xs
                  | otherwise = inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRange lo hi xs == inRangeRec lo hi xs

-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
positives_numbers :: [Int] -> [Int]
positives_numbers xs = [x | x <- xs, x > 0]
countPositives xs = length (positives_numbers xs)

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec [x] | x > 0 = 1
                      | otherwise = 0
countPositivesRec (x:xs) | x > 0 = countPositivesRec xs + 1
                         | otherwise = countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives xs == countPositivesRec xs


-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount = undefined

-- List-comprehension version
pennypincher :: [Int] -> Int
objects_buy xs = [x  | x <- xs, x <= 19900]
pennypincher xs = sum (objects_buy xs)

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec [x] | x <= 19900 = x
                    | otherwise = 0
pennypincherRec (x:xs) | x <= 19900 = pennypincherRec xs + x
                       | otherwise = pennypincherRec xs

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs == pennypincherRec xs

-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits = undefined

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec = undefined

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits = undefined



-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise = undefined

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec = undefined

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise = undefined



-- 7. title

-- List-comprehension version
title :: [String] -> [String]
title = undefined

-- Recursive version
titleRec :: [String] -> [String]
titleRec = undefined

-- mutual test
prop_title :: [String] -> Bool
prop_title = undefined
