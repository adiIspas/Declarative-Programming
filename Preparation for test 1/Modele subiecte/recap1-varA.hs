module VarA where

-- 1
-- Lista divizorilor unui numar
-- Folosind recursivitate
-- divsorsR 6 = [1, 2, 3, 6]
divisors :: Int -> [Int]
divisors n = helper 1  -- start with candidate 1
  where helper x
          | n `mod` x == 0  = x : helper (x+1)
          | x > n           = []
          | otherwise          = helper (x+1)


-- 2
-- Lista divizorilor impari ai unui numar
-- Folosind list comprehension
oddDivisors :: Int -> [Int]
oddDivisors n = [x | x <- divisors n, x `mod` 2 == 1]


-- 3
-- Testare daca un numar este perfect, adica egal cu suma divizorilor fara el insusi
-- Fara recursie sau list comprehension
-- isPerfect 6 = True    pt ca 6  == 1 + 2 + 3
-- isPerfect 10 = False  pt ca 10 \= 1 + 2 + 5
isPerfect :: Int -> Bool
isPerfect n = if (foldr (+) 0 (divisors n) - n == n) then True else False


-- 4
-- Testarea daca suma diferentelor elementelor consecutive este un nr perfect
-- perfDiffs [3, 4, 9] = True       pt ca [3-4, 4-9] ~> [1, 5] ~> 6
-- perfDiffs [1, 5, 8, 20] = False  pt ca [5-1, 8-5, 20-8] ~> [4, 3, 12] ~> 19
perfDiffs :: [Int] -> Bool
perfDiffs xs = isPerfect (foldr (+) 0 (diff xs))

diff :: [Int] -> [Int]
diff [] = []
diff xs | length xs > 1 = (-1) * (head xs - (head (tail xs))) : diff (tail xs)
        | otherwise = []
