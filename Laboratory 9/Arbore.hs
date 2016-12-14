module Arbore (Tree,
               adauga,
               cauta,
               ini,
               parcurgere)
where


data Tree a = Leaf
            | Node a (Tree a) (Tree a)
            deriving Show

-- tree for testing
root :: Tree Int
root = (Node 7 (Node 3 (Node 1 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 10 Leaf Leaf))
--        7
--      /   \
--     3     10
--    / \
--   1   5

adauga :: Ord a => a -> Tree a -> Tree a
adauga nod Leaf = Node nod Leaf Leaf
adauga nod (Node c_nod stanga dreapta) | nod <= c_nod = Node c_nod (adauga nod stanga) dreapta
                                       | otherwise = Node c_nod stanga (adauga nod dreapta)

cauta :: Ord a => a -> Tree a -> Maybe a
cauta nod Leaf = Nothing
cauta nod (Node c_nod Leaf Leaf) = if nod == c_nod then Just nod else Nothing
cauta nod (Node c_nod stanga dreapta) | nod < c_nod = cauta nod stanga
                                      | nod > c_nod = cauta nod dreapta
                                      | otherwise = Just nod


ini :: Ord a => [a] -> Tree a
ini [] = Leaf
ini (nod:numere) = adauga nod (ini numere)

-- parcurgere (ini [1,5,2,8,10,3,11,6,7]) == [1,2,3,5,6,7,8,10,11]
parcurgere :: Tree a -> [a]
parcurgere Leaf = []
parcurgere (Node nod stanga dreapta) = (parcurgere stanga) ++ [nod] ++ (parcurgere dreapta)
