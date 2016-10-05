--ghc 8.0.1 /opt/ghc/8.0.1/lib/ghc-8.0.0.20160127/

main = do print $ "Leg1: " ++ show (leg1 5 4)
          print $ "Leg2: " ++ show (leg2 5 4)
          print $ "Hyp: " ++ show (hyp 5 4)
          print $ "Double: " ++ show (double 5)
          print $ "Square: " ++ show (square 7)
          print $ "Is triple: " ++ show (isTriple 4 5 6)
       
-- Exercise 3:

double :: Int -> Int
double x = x + x

square :: Int -> Int
square x = x * x

-- Exercise 4:

isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = square a + square b == square c


-- Exercise 5:

leg1 :: Int -> Int -> Int
leg1 x y = square x - square y

leg2 :: Int -> Int -> Int
leg2 x y = 2 * x * y

hyp :: Int -> Int -> Int
hyp x y = square x + square y


-- Exercise 6:

prop_triple :: Int -> Int -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)

