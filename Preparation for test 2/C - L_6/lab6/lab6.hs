-- Informatics 1 Functional Programming
-- Tutorial 6
--
-- Due: 12/13 November

import System.Random


-- Importing the keymap module

import KeymapList


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
--longestProductLen = maximum . map (length . fst . snd)
longestProductLen products = maximum [length prod | (prod, _) <- [item | (_, item) <- products]]

test :: [(Barcode, Item)] -> [Int]
test products = [length prod | (prod, _) <- [item | (_, item) <- products]]

formatLine :: Int -> (Barcode, Item) -> String
formatLine n (barcode, (prod, unit)) = barcode ++ "..." ++ prod ++ (replicate (n - length prod + 3) '.') ++ unit ++ "\n"

showCatalogue :: Catalogue -> String
showCatalogue catalogue = unlines (map (formatLine (longestProductLen list)) list)
              where
                list = toList catalogue

-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:list) = Just a

catMaybes :: [Maybe a] -> [a]
catMaybes [Nothing] = []
catMaybes [Just a] = [a]
catMaybes (x:xs) = maybeToList x ++ catMaybes xs

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems [] _ = []
getItems (x:xs) catalogue = catMaybes [get x catalogue] ++ (getItems xs catalogue)



-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
