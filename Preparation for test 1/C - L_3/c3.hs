not' :: Bool -> Bool
not' True = False
not' False = True

(&&&) :: Bool -> Bool -> Bool
True &&& arg = arg
False &&& _ = False

infixl 6 <+>
(<+>) :: Int -> Int -> Int
x <+> y = x + y + 1

-- Produsul scalar a doi vectori de aceasi lungime
dot :: Num a => [a] -> [a] -> a
dot xs yx = sum [x * y | (x,y) <- zip xs yx]

-- Cauta toate pozitiile dintr-o lista pe care apare un element
search :: Eq a => [a] -> a -> [Int]
search xs x = [i | (i,y) <- zip [0..] xs, y == x]
