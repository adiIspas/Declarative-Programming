-- Informatics 1 - Functional Programming
-- Tutorial 3
--
-- Week 5 - Due: 22/23 Oct.

import Data.Char
import Test.QuickCheck



-- 1. Map
-- a. (4 simboluri)
uppers :: String -> String
uppers = map toUpper

-- b. (7 simboluri)
doubles :: [Int] -> [Int]
doubles = map (*2)

-- c. (10 simboluri)
penceToPounds :: [Int] -> [Float]
--penceToPounds = map (\x -> fromIntegral x * 0.01)
penceToPounds = map $ (0.01*).fromIntegral

-- d. (11 simboluri)
uppers' :: String -> String
uppers' xs = [toUpper x | x <- xs]

-- (8 simboluri)
prop_uppers :: String -> Bool
prop_uppers xs = uppers xs == uppers' xs



-- 2. Filter
-- a. (4 simboluri)
alphas :: String -> String
alphas = filter isAlpha

-- b. (8 simboluri)
rmChar ::  Char -> String -> String
rmChar ch = filter (/=ch)

-- c. (8 simboluri)
above :: Int -> [Int] -> [Int]
above n = filter (>=n)

-- d. (13 simboluri)
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = filter (uncurry (/=))

-- e. (15 simboluri)
rmCharComp :: Char -> String -> String
rmCharComp ch str = [x | x <- str, x /= ch]

-- (11 simboluri)
prop_rmChar :: Char -> String -> Bool
prop_rmChar ch str = rmChar ch str == rmCharComp ch str



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

-- (7 simboluri)
upperChars' :: String -> String
upperChars' = map toUpper . filter isAlpha

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

-- (13 simboluri)
largeDoubles' :: [Int] -> [Int]
largeDoubles' = map (*2) . filter (>3)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

-- (11 simboluri) - It's bad ...
reverseEven' :: [String] -> [String]
reverseEven' = map reverse . filter (even.length)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

-- (7 simboluri)
productFold :: [Int] -> Int
productFold = foldr (*) 1

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.  (16 simboluri)
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) =  x && andRec xs

-- (7 simboluri)
andFold :: [Bool] -> Bool
andFold = foldr (&&) True

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- c.  (17 simboluri)
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

-- (8 simboluri)
concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.  (17 simboluri) - It's bad
rmCharsRec :: String -> String -> String
rmCharsRec [] str = str
rmCharsRec (c:cs) str = rmChar c $ rmCharsRec cs str

-- (6 simboluri)
rmCharsFold :: String -> String -> String
rmCharsFold = flip $ foldr rmChar

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str


type Matrix = [[Int]]

-- 5
-- a. (10 simboluri)
uniform :: [Int] -> Bool
uniform xs = all (== head xs) xs

-- b. (	 simboluri)
valid :: Matrix -> Bool
valid []     = False
valid (x:xs) = not (null x) && uniform (map length (x:xs))

-- 6.

-- 7.  (22 simboluri + 19 simboluri)  cu tot cu tratarea erorilor
plusM :: Matrix -> Matrix -> Matrix
plusM = undefined

-- 8. (23 simboluri + 15 simboluri)  cu tot cu tratarea erorilor
timesM :: Matrix -> Matrix -> Matrix
timesM = undefined
