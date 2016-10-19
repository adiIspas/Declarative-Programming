-- Declarative Programming
-- Lab 2
--

import Data.Char
import Data.List
import Test.QuickCheck


-- Optional Material

-- 8. crosswordFind

-- List-comprehension version

findPositionLeterRec :: String -> Char -> Int
findPositionLeterRec [] char = 0
findPositionLeterRec (x:xs) char | x /= char = findPositionLeterRec xs char + 1
                                 | otherwise = 0

findPositionWordRec :: [String] -> String -> Int
findPositionWordRec [] word = 0
findPositionWordRec (x:xs) word | x /= word = findPositionWordRec xs word + 1
                                 | otherwise = 0

crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind char int1 int2 xs = [x | x <- xs, findPositionWordRec xs x == int2 || findPositionLeterRec x char == int1]

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec char int1 int2 [] = []
crosswordFindRec char int1 int2 (x:xs) | findPositionWordRec xs x == int2 || findPositionLeterRec x char == int1 = x : crosswordFindRec char int1 int2 xs
                                       | otherwise = crosswordFindRec char int1 int2 xs

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind char int1 int2 xs = crosswordFind char int1 int2 xs == crosswordFindRec char int1 int2 xs

-- 9. search

-- List-comprehension version

search :: String -> Char -> [Char]
search xs char = [x | x <- xs, char == x]

-- Recursive version
searchRec :: String -> Char -> [Int]
searchRec [] char = []
searchRec (x:xs) char | char == x = 1 + length xs : searchRec xs char
                      | otherwise = searchRec xs char

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search = undefined


-- 10. contains

-- List-comprehension version
contains :: String -> String -> Bool
contains = undefined

-- Recursive version
containsRec :: String -> String -> Bool
containsRec [] second = False
containsRec first second | isPrefixOf second drop length first - 1 first = True
                         | otherwise = containsRec first second

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains = undefined
