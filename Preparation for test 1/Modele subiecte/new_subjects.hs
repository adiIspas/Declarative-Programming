import Data.Char
import Data.List
import Test.QuickCheck

-- 1) Reuniunea, intersectia, diferenta a doua multimi

reuniune :: [Int] -> [Int] -> [Int]
reuniune xs [] = xs
reuniune [] ys = ys
reuniune (x:xs) (y:ys) = [x,y] ++ reuniune xs ys
