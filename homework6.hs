--Notes froom class
{-
data AExpr = Var String | Num Integer | Add AExpr AExpr | Sub AExpr AExpr | Mul AExpr AExpr | Div AExpr AExpr
type Vars = String
type Env = [(Vars,Integer)]
-- (4*y - 6) / (2+x)

ex:: AExpr
ex = Div (Sub (Mul (Num 4)(Var "y") (Num 6))(Add (Num 2 (Var "x"))))

--Helper for Vars 
noDups :: (Eq a) => [a] -> [a]
noDups [] = []
--noDups (x:xs) = x : filter (/= x) (noDupsxs)
noDups (x:xs) = x : noDups (filter(/= x) xs)

noDups' :: (Eq a) => [a] -> [a]
noDups' xs = foldr (\x acc -> x : filter (/= x) acc) [] xs

vars :: AExpr -> [Vars]
vars (Var x) = [x]
vars (Num n) = []
vars (Add el e2) = noDups(vars e1 ++ vars e2)
vars (Sub el e2) = noDups(vars e1 ++ vars e2)
vars (Mul el e2) = noDups(vars e1 ++ vars e2)
vars (Div el e2) = noDups(vars e1 ++ vars e2)

lookUp :: vars -> [(Vars,Integer)] -> Integer
lookUp x  [] = error ("Variable not Found: " ++ x)
--lookUp x (e:es) = if x == fst e then snd e else lookUp x es
lookUp x ((var,val):es) | x == var = val
                        | Otherwise = lookUp x es

eval :: [(Vars,Integer)] -> AExpr -> Integer
eval (Var x) = lookUp x env
eval (Num n) = n
eval (Add el e2) = eval env e1 + eval env e2
eval (Sub el e2) = eval env e1 - eval env e2
eval (Mul el e2) = eval env e1 * eval env e2
eval (Div el e2) = eval env e1 'div' env eval e2

env1 :: Env
env1 = [("x",1), ("y",2)]
env2 :: Env
env3 = [("x",-1), ("y",10)]
env3 :: Env
env3 = [("x",5), ("y",20), ("z",100)]

-}

import Data.List (nub)
import Data.List (union)





------------------WARM UP QUESTIONS-----------------------
mapAppend :: (a -> [b]) -> [a] -> [b]
mapAppend _ [] = []
mapAppend f (x:xs) = f x ++ mapAppend f xs

helperAddLetter :: Char -> String -> String
helperAddLetter x str = x : str

addLetter :: Char -> [String] -> [String]
addLetter c [] = []
addLetter c arr = map (helperAddLetter c) arr

addLetters :: [Char] -> [String] -> [String]
addLetters ch arr = concatMap (`addLetter` arr) ch

makeWords :: [Char] -> Integer -> [String]
makeWords chars n = foldr (\_ acc -> [x:y | x <- chars, y <- acc]) [""]
                     [1..n]

update :: (Eq a) => (a,b) -> [(a,b)] -> [(a,b)]
update (key,val) [] = [(key,val)]
update (key,val) ((k,v):lst) | key == k = (key,val) : lst
                             | otherwise = (k,v) : update (key,val) lst 

--Try again: 
update' :: (Eq a) => (a,b) -> [(a,b)] -> [(a,b)]
update' (key,val) [] = [(key,val)]
update' (key,val) ((x,y):lst) | key == x = (key,val) : lst
                              | otherwise = (x,y) : update' (key,val) lst

--------------------HW---------------------------


type Vars   = String
type Env    = [(Vars,Bool)]
data Prop   = Var Vars | Const Bool | And Prop Prop | Or Prop Prop | Not Prop
            deriving (Show,Eq)
prop1 = Var "X" `And` Var "Y" -- X /\ Y
prop2 = Var "X" `Or` Var "Y" -- X \/ Y
prop3 = Not (Var "X") `Or` (Var "Y") -- !X \/ Y
prop4 = Not (Var "X") `Or` Not (Var "Y") -- !X \/!Y
{-   And
    /   \
  X      Y
      Or
    /   \
  X      Y
       Or
      /   \
    Not    Y
     |
     X
       Or
      /   \
    Not    Not
     |      |
     X      Y
-}

fv :: Prop -> [Vars]
fv (Var x)      = [x]
fv (Const _)    = []
fv (Not p)      = fv p
fv (And p q)    = fv p `union` fv q
fv (Or p q)     = fv p `union` fv q

countOccurs :: Vars -> Prop -> Integer
countOccurs x (Var y)   = if x == y then 1 else 0
countOccurs x (Not p)   = countOccurs x p
countOccurs x (And p q) = countOccurs x p + countOccurs x q
countOccurs x (Or p q)  = countOccurs x p + countOccurs x q

setTrue :: Vars -> Prop -> Prop
setTrue x (Var y)      = if x == y then Const True else Var y
setTrue x (Not p)      = Not (setTrue x p)
setTrue x (And p q)    = And (setTrue x p) (setTrue x q)
setTrue x (Or p q)     = Or (setTrue x p) (setTrue x q)
setTrue x (Const b)    = Const b

lookUp :: Vars -> Env -> Bool
lookUp _ [] = error "Not found"
lookUp x ((v,b):xs) = if x == v then b else lookUp x xs

eval :: [(Vars, Bool)] -> Prop -> Bool
eval env (Var y)     = lookUp y env
eval env (Not p)     = not (eval env p)
eval env (And p q)   = eval env p && eval env q
eval env (Or p q)    = eval env p || eval env q
eval _ (Const b)     = b

evalList :: Prop -> [Env] -> Bool

evalList x y = any (`eval` x) y

extendEnv :: [Env] -> Vars -> [Env]
extendEnv envs var = [ (var, b) : env | b <- [False, True], env <- envs ]

genEnvs :: [Vars] -> [Env]
genEnvs [] = [[]]
genEnvs (v:vs) = 
  [ (v, b) : env | b <- [False, True], env <- genEnvs vs ]

varsInProp :: Prop -> [Vars]
varsInProp (Var x)       = [x]
varsInProp (Const _)     = []
varsInProp (Not p)       = varsInProp p
varsInProp (And p q)     = nub (varsInProp p ++ varsInProp q)
varsInProp (Or p q)      = nub (varsInProp p ++ varsInProp q)
sat :: Prop -> Bool
sat p = evalList p (genEnvs(varsInProp p))

checkEq :: Prop -> Prop -> Bool
checkEq f1 f2 = 
    let vars = nub (varsInProp f1 ++ varsInProp f2)
        envs = genEnvs vars 
    in all (\env -> eval env f1 == eval env f2) envs 
