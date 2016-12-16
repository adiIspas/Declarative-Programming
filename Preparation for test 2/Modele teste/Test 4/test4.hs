data Ansamblu a = Nil
                | Nod a (Ansamblu a) (Ansamblu a) Int
                deriving (Show)

ansamblu = Nod 1 (Nod 3 (Nod 7 Nil Nil 1) (Nod 5 Nil Nil 1) 3) (Nod 2 Nil Nil 1) 5

empty :: Ansamblu a
empty = Nil

nullAnsamblu :: Ansamblu a -> Bool
nullAnsamblu heap = case heap of Nil -> True
                                 _   -> False

headAnsamblu :: Ansamblu a -> a
headAnsamblu Nil = error "Heap-ul e gol"
headAnsamblu (Nod element _ _ _) = element

lengthAnsamblu :: Ansamblu a -> Int
lengthAnsamblu Nil = 0
lengthAnsamblu (Nod _ _ _ lungime) = lungime
