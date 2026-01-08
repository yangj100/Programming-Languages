import Data.Char
import Data.List

-- Interpreter for STLC extended with lists and unit type.

-- Part 1. Datatype representation
-- Haskell datatypes should closely mirror the abstract syntax of terms and types

type Vars = String
type TVars = String

--     T  ::=  Z  |   B   |     T -> T      |      T x T       |    [ T ]
data Types = Ints | Bools | Fun Types Types | Prod Types Types | List Types
           | Unit -- 1
           | TVar TVars
  deriving (Eq)

--     /\ ::=  \/ |  /\ /\  |  \ \/ . /\
data Terms = Var Vars | App Terms Terms | Abs Vars Terms
           | Null | Nil | Cons Terms Terms | Fold Terms Terms Terms
         --  ()  |  []  |     /\ @ /\      | Fold (/\ , /\ , /\)
  deriving (Show,Eq)

data Token = LPar | RPar | VSym Vars | Dot | Backslash
           | NullC | NilC | ConsOp | FoldOp | Comma
           | Err String | PT Terms
  deriving (Show,Eq)

-- These type synonyms are used in the implementation of type inference
type Constr = (Types,Types)
type Cxt = [(Vars,Types)]
type TSub = [(TVars,Types)]

-- A pretty printer for types (optional)
instance Show Types where
  show Ints = "Z"
  show Bools = "B"
  show Unit = "1"
  show (Fun a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (Prod a b) = "(" ++ show a ++ " x " ++ show b ++ ")"
  show (List a) = "[" ++ show a ++ "]"
  show (TVar a) = a

-- Part 2. Lexer and parser
-- The lexer should account for each token, except special tokens used by the parser

lexer :: String -> [Token]
lexer "" = []
-- Punctuation
lexer xs | take 4 xs == "fold" = FoldOp : lexer (drop 4 xs)
lexer ('(':')':xs) = NullC : lexer xs
lexer ('[':']':xs) = NilC : lexer xs
lexer ('@':xs) = ConsOp : lexer xs
lexer ('(':xs) = LPar : lexer xs
lexer (')':xs) = RPar : lexer xs
lexer ('.':xs) = Dot : lexer xs
lexer (',':xs) = Comma : lexer xs
lexer ('\\':xs) = Backslash : lexer xs
-- Variables start with lowercase, followed by letters or numbers
lexer (x:xs) | isSpace x = lexer xs
lexer (x:xs) | isLower x = VSym v : lexer r where (v,r) = span isAlphaNum (x:xs)
lexer xs = [Err (take 10 xs)]

sr :: [Token] -> [Token] -> [Token]
-- grammar rules
sr (NullC : s) q = sr (PT Null : s) q
sr (NilC : s) q = sr (PT Nil : s) q
sr (PT t : ConsOp : PT h : s) q = sr (PT (Cons h t) : s) q
sr (RPar : PT u : Comma : PT t : Comma : PT r : LPar : FoldOp : s) q
  = sr (PT (Fold r t u) : s) q
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

-- Part 3. Auxiliary functions
-- These include variable extraction and substitution inside types and terms

-- A list containing infinitely many variables
allVars :: [Vars]
allVars = tail vars where
  vars = "" : expand vars
  expand (v:vs) = map (\c -> v ++ [c]) ['a'..'z'] ++ expand vs

-- A function generating new variables outside the given list
freshVar :: [Vars] -> Vars
freshVar xs =
  let is = map (\x -> elemIndex x allVars) xs -- list of Maybe Int
      maxIndex [] = 0
      maxIndex (Nothing:ns) = maxIndex ns
      maxIndex (Just n:ns) = max n (maxIndex ns)
   in allVars !! (maxIndex is  + 1)

-- The function extracting term variables from a term
fv :: Terms -> [Vars]
fv (Var x) = [x]
fv (App s t) = fv s ++ fv t
fv (Abs x t) = filter (/= x) (fv t)
fv (Nil) = []
fv (Null) = []
fv (Cons x t) = fv x ++ fv t
fv (Fold s t u) = fv s ++ fv t ++ fv u

-- The function extracting type variables from a type
getTVars :: Types -> [TVars]
getTVars Ints = []
getTVars Bools = []
getTVars (Fun a b) = getTVars a ++ getTVars b
getTVars (Prod a b) = getTVars a ++ getTVars b
getTVars (List a) = getTVars a
getTVars (TVar x) = [x]

-- The function extracting type variables from a context
getTVarsCxt :: Cxt -> [TVars]
getTVarsCxt [] = []
getTVarsCxt ((x,a):g) = getTVars a ++ getTVarsCxt g

-- substituting variable inside a term
subst :: (Vars,Terms) -> Terms -> Terms
subst (x,t) (Var y) = if x==y then t else (Var y)
subst (x,t) (Fold s1 s2 s3) = Fold (subst (x,t) s1) (subst (x,t) s2) (subst (x,t) s3)
subst (x,t) (Cons s1 s2) = Cons (subst (x,t) s1) (subst (x,t) s2)
subst (x,t) (App s1 s2) = App (subst (x,t) s1) (subst (x,t) s2)
subst (x,t) (Abs y r)
  | not (elem y (fv t)) = Abs y (subst (x,t) r)
  | otherwise = let z = freshVar (x : y : fv t ++ fv r)
                    r' = subst (y,Var z) r
                 in Abs z (subst (x,t) r')
subst (x,t) c = c

-- substituting type variable inside a type
tsubst :: (TVars,Types) -> Types -> Types
tsubst (a,r) Ints = Ints
tsubst (a,r) Bools = Bools
tsubst (a,r) (Fun t1 t2) = Fun (tsubst (a,r) t1) (tsubst (a,r) t2)
tsubst (a,r) (Prod t1 t2) = Prod (tsubst (a,r) t1) (tsubst (a,r) t2)
tsubst (a,r) (List t) = List (tsubst (a,r) t)
tsubst (a,r) (TVar b) = if a==b then r else (TVar b)

-- substituting type variable inside a constraint
csubst :: (TVars,Types) -> Constr -> Constr
csubst (a,r) (lhs,rhs) = (tsubst (a,r) lhs, tsubst (a,r) rhs)

-- applying a list of type substitutions, in sequence, to a given type
tsubstList :: TSub -> Types -> Types
tsubstList tsub t = foldl (flip tsubst) t tsub

-- Part 4. Type inference

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
genConstrs g (Null) a = [(a,Unit)]
genConstrs g (Nil) a =
  let tvars = getTVars a ++ getTVarsCxt g
      a1 = freshVar tvars
   in [(a,List (TVar a1))]
genConstrs g (Cons x xs) a =
  let tvars = getTVars a ++ getTVarsCxt g
      a1 = freshVar tvars
      cs1 = genConstrs g x (TVar a1)
      cs2 = genConstrs g xs (List (TVar a1))
   in (a,List (TVar a1)) : cs1 ++ cs2
genConstrs g (Fold r s t) a =
  let tvars = getTVars a ++ getTVarsCxt g
      a1 = freshVar tvars
      cs1 = genConstrs g r (Fun (TVar a1) (Fun a a))
      cs2 = genConstrs g s a
      cs3 = genConstrs g t (List (TVar a1))
   in cs1 ++ cs2 ++ cs3

-- Phase 2: Solve the constraints
unify :: [Constr] -> TSub
unify [] = []
unify ((lhs,rhs) : cs) | lhs == rhs = unify cs
unify ((TVar a,rhs) : cs)
  | elem a (getTVars rhs) = error ("Circular type: " ++ show (a,rhs))
  | otherwise = (a,rhs) : unify (map (csubst (a,rhs)) cs) -- substitute a with rhs everywhere!
unify ((lhs,TVar a) : cs) = unify ((TVar a,lhs):cs)
unify ((Fun a1 a2 , Fun b1 b2) : cs) = unify ((a1,b1) : (a2,b2) : cs)
unify ((List a, List b) : cs) = unify ((a,b) : cs)
unify (c:cs) = error ("Type error:" ++ show c)

-- Phase 3: Apply the generated type substitutions
solve :: String -> Types
solve s = case sr [] (lexer s) of
  [PT t] -> tsubstList (unify (genConstrs [] t (TVar "a"))) (TVar "a")
  stack  -> error $ "Parse error: " ++ show stack

-- Part 4. Reduction.

red :: Terms -> Terms
-- reduction rules: these should always come first
red (App (Abs x s) t) = subst (x,t) s
red (Fold r s Nil) = s
red (Fold r s (Cons h t)) = App (App r h) (Fold r s t)
-- congruence rules: search for a redex recursively through the subtrees
red (App s t) = App (red s) (red t)
red (Cons s t) = Cons (red s) (red t)
red (Fold s t u) = Fold (red s) (red t) (red u)
red (Abs x t) = Abs x (red t)
-- base case: constants and variables don't reduce
red s = s

-- loop until no more progress is being made
nf :: Terms -> Terms
nf t = if t' == t then t else nf t' where t' = red t
