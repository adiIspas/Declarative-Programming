import Data.List

-- Tipul de date Exp
data Exp = Lit Int
         | Add Exp Exp
         | Mul Exp Exp
         | Exp :+: Exp
         | Exp :*: Exp

-- Evaluarea expresiei
evalExp :: Exp -> Int
evalExp (Lit n) = n
evalExp (Add exp1 exp2) = evalExp exp1 + evalExp exp2
evalExp (Mul exp1 exp2) = evalExp exp1 * evalExp exp2
evalExp (exp1 :+: exp2) = evalExp exp1 + evalExp exp2
evalExp (exp1 :*: exp2) = evalExp exp1 * evalExp exp2

-- Afisarea expresiei
showExp :: Exp -> String
showExp (Lit n) = show n
showExp (Add exp1 exp2) = par (showExp exp1 ++ " + " ++ showExp exp2)
showExp (Mul exp1 exp2) = par (showExp exp1 ++ " * " ++ showExp exp2)
showExp (exp1 :+: exp2) = par (showExp exp1 ++ " + " ++ showExp exp2)
showExp (exp1 :*: exp2) = par (showExp exp1 ++ " * " ++ showExp exp2)

-- Functie auxiliara
par :: String -> String
par s = "(" ++ s ++ ")"

-- Testare
e0, e1, e3, e4, e5 :: Exp
e0 = Add (Lit 2) (Mul (Lit 3) (Lit 3))
e1 = Mul (Add (Lit 2) (Lit 3)) (Lit 3)
e3 = (Lit 2) :+: ((Lit 3) :*: (Lit 3))
e4 = ((Lit 2) :+: (Lit 3)) :*: (Lit 3)
e5 = Mul ((Lit 1) :+: (Lit 2)) (Lit 3)

-- Tipul de date propozitie
type Name = String
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop
          deriving (Eq, Ord)

type Names = [Name]
type Env = [(Name,Bool)]

-- Afisare propozitie
showProp :: Prop -> String
showProp (Var x) = x
showProp F = "F"
showProp T = "T"
showProp (Not p) = par ("~" ++ showProp p)
showProp (p :|: q) = par (showProp p ++ "|" ++ showProp q)
showProp (p :&: q) = par (showProp p ++ "&" ++ showProp q)

-- Multimea variabilelor dintr-o propozitie
names :: Prop -> Names
names (Var x) = [x]
names F = []
names T = []
names (Not p) = names p
names (p :|: q) = nub (names p ++ names q)
names (p :&: q) = nub (names p ++ names q)


-- Evaluarea expresiilor
eval :: Env -> Prop -> Bool
eval e (Var x) = lookUp e x
eval e F = False
eval e T = True
eval e (Not p) = not (eval e p)
eval e (p :|: q) = eval e p || eval e q
eval e (p :&: q) = eval e p && eval e q

lookUp :: Eq a => [ ( a , b ) ] -> a -> b
lookUp xys x = the [ y | ( x', y ) <- xys , x == x']
    where
    the [ x ] = x

-- Evaluare, exemplu
p :: Prop
p = ( Var " a " :&: Not ( Var " a " ) )
e :: Env
e = [ ( " a " , True ) ]

-- Generarea tuturor valuatiilor
envs :: Names -> [Env]
envs [] = [[]]
envs (x:xs) = [(x,False):e | e <- envs xs] ++ [(x,True):e | e <- envs xs]

-- Satisfiabilitate
