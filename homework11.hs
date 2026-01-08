import Data.Char (isSpace, isLower, isAlphaNum, isDigit)
import System.IO (hFlush, stdout)
import Prelude hiding ((<$>))
import Data.Functor ( (<$>) )
import Data.List
import System.Exit (exitSuccess, exitFailure)



type Vars  = String
type TVars = String

data Types
  = Ints
  | Fun Types Types
  | TVar TVars
  deriving (Show, Eq)

data Terms
  = Var Vars
  | App Terms Terms
  | Abs Vars Terms
  | Num Integer
  | Sub Terms Terms
  | IfPos Terms Terms Terms
  | Y
  deriving (Show, Eq)

data Token
  = VSym String
  | CSym Integer
  | SubOp
  | IfPositiveK
  | ThenK
  | ElseK
  | YComb
  | LPar
  | RPar
  | Dot
  | Backslash
  | Err String
  | PT Terms
  deriving (Show, Eq)

-- Part 2.1: Lexer

lexer :: String -> [Token]
lexer "" = []
lexer ('-':xs) = SubOp : lexer xs
lexer (x:xs)
  | isSpace x  = lexer xs
  | isLower x  =
      let (v, r) = span isAlphaNum (x:xs)
      in case v of
           "ifPositive" -> IfPositiveK : lexer r
           "then"       -> ThenK        : lexer r
           "else"       -> ElseK        : lexer r
           _            -> VSym v       : lexer r
lexer ('(':xs) = LPar      : lexer xs
lexer (')':xs) = RPar      : lexer xs
lexer ('.':xs) = Dot       : lexer xs
lexer ('\\':xs)= Backslash : lexer xs
lexer (x:xs)
  | isDigit x  =
      let (num, rest) = span isDigit (x:xs)
      in CSym (read num) : lexer rest
lexer ('Y':xs) = YComb     : lexer xs
lexer xs       = [Err (take 10 xs)]

-- Part 2.2: Parser

parser :: [Token] -> Either Terms String
parser ts = case sr [] ts of
  [PT t] -> Left t
  stk    -> Right ("Parse error: " ++ show stk)

sr :: [Token] -> [Token] -> [Token]
sr (VSym x : s) q                        = sr (PT (Var x) : s) q
sr (CSym n : s) q                        = sr (PT (Num n) : s) q
sr (YComb   : s) q                       = sr (PT Y       : s) q
sr (PT t2 : SubOp : PT t1 : s) q         = sr (PT (Sub t1 t2) : s) q
sr (PT t2 : PT t1 : s) q                 = sr (PT (App t1 t2) : s) q
sr (PT t2 : Dot : PT (Var x) : Backslash : s) q
  = sr (PT (Abs x t2) : s) q
sr (PT u : ElseK : PT t : ThenK : PT s' : IfPositiveK : sstk) q
  = sr (PT (IfPos s' t u) : sstk) q
sr (RPar : PT t : LPar : s) q            = sr (PT t : s) q
sr (Err e : _) _                         = [Err e]
sr s (c:q)                               = sr (c:s) q
sr s []                                  = s

-- Part 3: Typing

type Constr = (Types,Types)
type Cxt    = [(Vars,Types)]
type TSub   = [(TVars,Types)]

getTVars :: Types -> [TVars]
getTVars Ints       = []
getTVars (TVar a)   = [a]
getTVars (Fun x y)  = getTVars x ++ getTVars y

getTVarsCxt :: Cxt -> [TVars]
getTVarsCxt []             = []
getTVarsCxt ((_, t) : rest) = getTVars t ++ getTVarsCxt rest

allTVars :: [TVars]
allTVars = tail vars where
  vars = "" : expand vars
  expand (v:vs) = [ v ++ [c] | c <- ['a'..'z'] ] ++ expand vs

freshVar :: [TVars] -> TVars
freshVar xs =
  let idxs = map (`elemIndex` allTVars) xs
      maxIdx []            = -1
      maxIdx (Nothing:ns)  = maxIdx ns
      maxIdx (Just i:ns)   = max i (maxIdx ns)
  in allTVars !! (maxIdx idxs + 1)

tsubst :: (TVars,Types) -> Types -> Types
tsubst (_, r) Ints        = Ints
tsubst (a, r) (TVar b)
  | a == b                = r
  | otherwise             = TVar b
tsubst s       (Fun x y)  = Fun (tsubst s x) (tsubst s y)

class CSubst a where
  csubst :: (TVars,Types) -> a -> a

instance CSubst Constr where
  csubst s (l,r) = (tsubst s l, tsubst s r)

instance CSubst [Constr] where
  csubst s = map (csubst s)

genConstrs :: Cxt -> Terms -> Types -> [Constr]
genConstrs ctx (Var x) a = case lookup x ctx of
  Just b  -> [(a,b)]
  Nothing -> error $ "Unbound variable: " ++ x

genConstrs ctx (App s t) a =
  let used = getTVars a ++ getTVarsCxt ctx
      a1   = freshVar used
  in genConstrs ctx s (Fun (TVar a1) a)
     ++ genConstrs ctx t (TVar a1)

genConstrs ctx (Abs x t) a =
    let used = getTVars a ++ getTVarsCxt ctx
        a1   = freshVar used
        a2   = freshVar (a1 : used)
    in  (a, Fun (TVar a1) (TVar a2))
      : genConstrs ((x, TVar a1) : ctx) t (TVar a2)

genConstrs _   (Num _) a = [(a,Ints)]
genConstrs ctx (Sub s u) a = (a,Ints)
                             : genConstrs ctx s Ints
                            ++ genConstrs ctx u Ints
genConstrs ctx (IfPos r s t) a =
  genConstrs ctx r Ints
  ++ genConstrs ctx s a
  ++ genConstrs ctx t a
genConstrs ctx Y a =
  let used = getTVars a ++ getTVarsCxt ctx
      a1   = freshVar used
  in [(a, Fun (Fun (TVar a1) (TVar a1)) (TVar a1))]

tsubstList :: TSub -> Types -> Types
tsubstList subs t = foldl (flip tsubst) t subs

unify :: [Constr] -> TSub
unify [] = []
unify ((l,r):cs)
  | l == r                = unify cs
unify ((TVar a,r):cs)
  | a `elem` getTVars r   = error $ "Circular type: " ++ a
  | otherwise             = (a,r) : unify (map (csubst (a,r)) cs)
unify ((l,TVar a):cs) = unify ((TVar a,l):cs)
unify ((Fun x1 x2, Fun y1 y2):cs) =
  unify ((x1,y1):(x2,y2):cs)
unify (c:_) = error $ "Type error: " ++ show c

infer :: Terms -> Types
infer t =
  let cs  = genConstrs [] t (TVar "a")
      sub = unify cs
  in tsubstList sub (TVar "a")

-- Part 4: Reduction

fv :: Terms -> [Vars]
fv (Var x)       = [x]
fv (Num _)       = []
fv Y             = []
fv (App s t)     = fv s ++ fv t
fv (Sub s u)     = fv s ++ fv u
fv (IfPos r s t) = fv r ++ fv s ++ fv t
fv (Abs x t)     = filter (/= x) (fv t)

subst :: (Vars,Terms) -> Terms -> Terms
subst (x,u) v@(Var y)
  | x == y    = u
  | otherwise = v
subst s (App t1 t2)   = App   (subst s t1) (subst s t2)
subst s (Sub t1 t2)   = Sub   (subst s t1) (subst s t2)
subst s (IfPos r t e) = IfPos (subst s r) (subst s t) (subst s e)
subst s@(x,u) (Abs y t)
  | x == y            = Abs y t
  | y `elem` fv u     =
      let z  = freshVar (fv u ++ fv t)
          t' = subst (y,Var z) t
      in Abs z (subst s t')
  | otherwise         = Abs y (subst s t)
subst _ Y             = Y
subst _ n@(Num _)     = n

red :: Terms -> Terms
red (App (Abs x s) t)      = subst (x,t) s
red (Sub (Num m) (Num n))  = Num (m - n)
red (IfPos (Num n) s t)
  | n > 0                   = s
  | otherwise               = t
red (App(App Y f) x)       = App f (App (App Y f) x)
red (App Y t)              = App Y t
red (App s t)              = App (red s) (red t)
red (Sub s t)              = Sub (red s) (red t)
red (IfPos r s t)          = IfPos (red r) (red s) (red t)
red (Abs x t)              = Abs x (red t)
red t                      = t

reds :: Terms -> Terms
reds = go 1000
  where
    go :: Int -> Terms -> Terms
    go 0 t = t    
    go n t =
      let t' = red t
      in if t' == t 
           then t  
           else go (n - 1) t'



main :: IO ()
main = do
  putStrLn "Enter input filename:"
  fname  <- getLine
  source <- readFile fname

  case parser (lexer source) of
    Right err -> do
      putStrLn $ "Parse error: " ++ err
      -- for a parse error you can either:
      --   return ()           -- just stop here
      -- or
      --   exitFailure         -- terminate with non‐zero code
      return ()

    Left term -> do
      -- 1) type inference
      let ty = infer term
      putStrLn $ "Inferred type: " ++ show ty

      -- 2) reduction
      let nf = reds term
      putStrLn $ "Normal form:   " ++ show nf

      -- and then just return; don't call exitSuccess
      return ()



