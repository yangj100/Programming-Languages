-- We are making a hackell in hackell -- Last HW ---

-- Part 1: We can't use the actual Int or Bool because it's taken
--     T ::=  Z   |   B   |    T ->   T  |     T x T |            [T]
data Types = Ints | Bools | Fun Types Types | Prod Types Types | List Types

-- /\ ::= \/ | /\ /\ | \ \/ . /\
data Types = Var Vars | App Terms Terms | Abs Vars Terms | 

-- Lexer:
--Number 6 on HW 3.
ex :: String
ex = "\\x.x(\\y.yx))(\\z.z)(\\y.yy)"

data Token = LPar | RPar | VSym Vars | Dot | Backslash 
            | Err String | PT Terms
            deriving (show,eq)

red :: Terms -> Terms 
red (App (Abs x s) t) = subst (x,t)s 
red (App s t) = App (red s) (red t)
red (Abs x t) = Abs x (red t)
red s = s

subst :: (Vars, Terms) -> Terms -> Terms
subst (x,t) (Var y) = if x ==y then t else (var y)
subst (x,t) (App s1 s2) = App (subst (x,t) s1) (subst (x,t) s2)
subst (x,t) (Abs y r)
    | not (elem y (fv t)) = Abs y (subst (x,t) r)
    | otherwise = let z = error ""
                      r' = subst (y,Var,z)r
                      in Abs z (subst (x,t) r')

allVars :: [Vars]
allVars = tail vars where
    vars = "" : expand vars 
    expand (v:vs) = map (\c ->v ++ [c]) ['a'..'z'] ++ expand vs
lexer :: String -> [Token]
lexer "" = []
lexer ('(':xs) = LPar :lexer xs 
lexer (')':xs) = RPar :lexer xs
lexer ('.':xs) = Dot : lexer xs 
lexer ('\\':xs) = Backslash : lexer xs
--Variables start with lowercase, followed by letters or numbers
lexer ()
lexer ('\\':xs) | isLower x = VSym: lexer r where (v,r) = span is AlphaNum(x:xs)
lexer xs = Err (take 10 xs)

sr :: [Token] -> [Token] -> [Token]
--grammar rules
sr(VSym x : s) q = sr(PT (Var x ): s)q
sr (Pt t2 PT t1 s) q = sr(PT (App t1 t2) :s)q
sr (PT t : Dot : PT (Var x) : Backslash) :s) q@(RPar: _) = sr (PT(Abs x t) : s)q -- The same as abs: /\ -> \ \/./\ 
sr (PT t : Dot : PT (Var x) : Backslash) :s) [] = sr (PT(Abs x t) : s) []
-- base case
sr (Err e : s) q = [Err e]
sr s (c:q) = sr (c:s) q 
sr s [] = s

-- if dont match any of the greammar rules, you put it on the stack 



--Goal: 
--We want to take a string and actually construct
parseTerm :: String -> Either Terms String






















-- THE END