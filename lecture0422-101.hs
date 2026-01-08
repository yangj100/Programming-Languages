import Data.Char
-- Interpreter for PCF, the bootstrap

-- Part 1. Data representation
type Vars = String
type TVars = String

--     T  ::=  Z  |   B   |     T -> T      |      T x T       |    [ T ]
data Types = Ints | Bools | Fun Types Types | Prod Types Types | List Types
           | TVar TVars
  deriving (Show,Eq)

--     /\ ::=  \/ |  /\ /\  |  \ \/ . /\
data Terms = Var Vars | App Terms Terms | Abs Vars Terms
  deriving (Show,Eq)

data Token = LPar | RPar | VSym Vars | Dot | Backslash
           | Err String | PT Terms
  deriving (Show,Eq)

-- Implementation of type inference!
type Constr = (Types,Types)
type Cxt = [(Vars,Types)]
type TSub = [(TVars,Types)]

getTVars :: Types -> [TVars]
getTVars Ints = []
getTVars Bools = []
getTVars (Fun a b) = getTVars a ++ getTVars b
getTVars (Prod a b) = getTVars a ++ getTVars b
getTVars (List a) = getTVars a
getTVars (TVar x) = [x]

getTVarsCxt :: Cxt -> [TVars]
getTVarsCxt [] = []
getTVarsCxt ((x,a):g) = getTVars a ++ getTVarsCxt g

-- Phase 1: Generate the constraints
-- The function works with a given context, term, and type (G |- t : A)
-- outputs a list of constraints needed for this type judgment to be valid
genConstrs :: Cxt -> Terms -> Types -> [Constr]
genConstrs g (Var x) a = case lookup x g of
    Just b  -> [(b,a)]
    Nothing -> error ("Variable not found in the context: " ++ x
                      ++ "\n Gamma = " ++ show g)
genConstrs g (App s t) a =
  let tvars = getTVars a ++ getTVarsCxt g
      a' = freshVar tvars
      cs1 = genConstrs g s (Fun (TVar a') a)
      cs2 = genConstrs g t (TVar a')
   in cs1 ++ cs2
genConstrs g (Abs x r) (Fun a b) = genConstrs ((x,a) : g) r b
genConstrs g (Abs x r) a =
  let tvars = getTVars a ++ getTVarsCxt g
      a1 = freshVar tvars
      a2 = freshVar (a1:tvars)
      g' = (x,TVar a1) : g
   in (a,Fun (TVar a1) (TVar a2)) : genConstrs g' r (TVar a2)


-- Phase 2: Solve the constraints

-- Phase 3: Apply the substitution



red :: Terms -> Terms
red (App (Abs x s) t) = subst (x,t) s
red (App s t) = App (red s) (red t)
red (Abs x t) = Abs x (red t)
red s = s

nf :: Terms -> Terms
nf t = if t' == t then t else nf t' where t' = red t

subst :: (Vars,Terms) -> Terms -> Terms
subst (x,t) (Var y) = if x==y then t else (Var y)
subst (x,t) (App s1 s2) = App (subst (x,t) s1) (subst (x,t) s2)
subst (x,t) (Abs y r)
  | not (elem y (fv t)) = Abs y (subst (x,t) r)
  | otherwise = let z = freshVar (x : y : fv t ++ fv r)
                    r' = subst (y,Var z) r
                 in Abs z (subst (x,t) r')

-- Variables
allVars :: [Vars]
allVars = tail vars where
  vars = "" : expand vars
  expand (v:vs) = map (\c -> v ++ [c]) ['a'..'z'] ++ expand vs

freshVar :: [Vars] -> Vars
freshVar xs = head (filter (\x -> not (elem x xs)) allVars)

fv :: Terms -> [Vars]
fv (Var x) = [x]
fv (App s t) = fv s ++ fv t
fv (Abs x t) = filter (/= x) (fv t)

ex :: String
ex = "(\\x.x(\\y.y x))(\\z.z)(\\y.y y)"

-- Part 2. Lexer and parser
lexer :: String -> [Token]
lexer "" = []
-- Punctuation
lexer ('(':xs) = LPar : lexer xs
lexer (')':xs) = RPar : lexer xs
lexer ('.':xs) = Dot : lexer xs
lexer ('\\':xs) = Backslash : lexer xs
-- Variables start with lowercase, followed by letters or numbers
lexer (x:xs) | isSpace x = lexer xs
lexer (x:xs) | isLower x = VSym v : lexer r where (v,r) = span isAlphaNum (x:xs)
lexer xs = [Err (take 10 xs)]

sr :: [Token] -> [Token] -> [Token]
-- grammar rules
sr (VSym x : s) q = sr (PT (Var x) : s) q
sr (PT t2 : PT t1 : s) q = sr (PT (App t1 t2) : s) q
sr (PT t : Dot : PT (Var x) : Backslash : s) q@(RPar : _) = sr (PT (Abs x t) : s) q
sr (PT t : Dot : PT (Var x) : Backslash : s) [] = sr (PT (Abs x t) : s) []
-- parens
sr (RPar : PT t : LPar : s) q = sr (PT t : s) q
-- base case
sr (Err e : s) q = [Err e]
sr s (c:q) = sr (c:s) q
sr s []    = s









-- parseTerm :: [Token] -> Either Terms String
