import Data.Char
type Vars = String -- Variables
type Value = Integer -- Values
type Env = [(Vars,Value)]
data AExpr = Var Vars | Num Value -- Arithmetic expressions
           | Add AExpr AExpr | Sub AExpr AExpr | Mul AExpr AExpr
           | Div AExpr AExpr | Mod AExpr AExpr | Exp AExpr AExpr
    deriving (Eq,Show)
data BExpr = Const Bool -- Boolean expressions
           | And BExpr BExpr | Or BExpr BExpr | Not BExpr
           | Eq AExpr AExpr | Lt AExpr AExpr | Lte AExpr AExpr
           | Neq AExpr AExpr | Gt AExpr AExpr | Gte AExpr AExpr
    deriving (Eq,Show)
data Instr = Assign Vars AExpr -- assignment
           | IfThen BExpr Instr -- conditional
           | IfThenElse BExpr Instr Instr -- another conditional
           | While BExpr Instr -- looping construct
           | Do [Instr] -- a block of several instructions
           | Nop -- the "do nothing" instruction
           | Return AExpr -- the final value to return
    deriving (Eq,Show)
data Keywords = IfK | ThenK | ElseK | WhileK | NopK | ReturnK
    deriving (Eq,Show)
data BOps = AddOp | SubOp | MulOp | DivOp | ModOp | ExpOp
          | AndOp | OrOp | EqOp | NeqOp
          | LtOp | LteOp | GtOp | GteOp
    deriving (Eq,Show)
data Token = VSym String | CSym Integer | BSym Bool
          | LPar | RPar | LBra | RBra | Semi
          | BOp BOps | NotOp | AssignOp
          | Keyword Keywords
          | Err String
          | PA AExpr | PB BExpr | PI Instr | Block [Instr]
    deriving (Eq,Show)

lexer :: String -> [Token]
lexer [] = []
lexer xs | take 2 xs == "if"  = Keyword IfK  : lexer (drop 2 xs)
lexer xs | take 4 xs == "then" = Keyword ThenK : lexer (drop 4 xs)
lexer xs | take 4 xs == "else" = Keyword ElseK : lexer (drop 4 xs)
lexer xs | take 5 xs == "while" = Keyword WhileK : lexer (drop 5 xs)
lexer xs | take 3 xs == "not" = Keyword NopK : lexer (drop 3 xs)
lexer xs | take 6 xs == "return" = Keyword ReturnK : lexer (drop 6 xs)
lexer (x:xs) | isDigit x = CSym (read start) : lexer end
  where (start,end) = span isDigit (x:xs)
lexer (x : xs) | isAlphaNum x = 
    let (start, end) = span isAlphaNum (x : xs) 
    in VSym start : lexer end
lexer xs | take 2 xs == "tt" = BSym (True) : lexer (drop 2 xs)
lexer xs | take 2 xs == "ff" = BSym (False) : lexer (drop 2 xs)
lexer ('(' : xs) = LPar : lexer xs
lexer (')' : xs) = RPar : lexer xs
lexer ('{' : xs) = LBra : lexer xs
lexer ('}' : xs) = RBra : lexer xs
lexer (';' : xs) = Semi : lexer xs
lexer (':' : '=' : xs) = AssignOp : lexer xs
lexer ('+' : xs) = BOp AddOp : lexer xs
lexer ('-' : xs) = BOp SubOp : lexer xs
lexer ('*' : xs) = BOp MulOp : lexer xs
lexer ('/' : xs) = BOp DivOp : lexer xs
lexer ('%' : xs) = BOp ModOp : lexer xs
lexer ('^' : xs) = BOp ExpOp : lexer xs
lexer ('&' : '&' : xs) = BOp AndOp : lexer xs
lexer ('|' : '|' : xs) = BOp OrOp : lexer xs
lexer ('=' : '=' : xs) = BOp EqOp : lexer xs
lexer ('!' : '=' : xs) = BOp NeqOp : lexer xs
lexer ('!' : xs) = NotOp : lexer xs
lexer ('<' : '=' : xs) = BOp LteOp : lexer xs
lexer ('<' : xs) = BOp LtOp : lexer xs
lexer ('>' : '=' : xs) = BOp GteOp : lexer xs
lexer ('>' : xs) = BOp GtOp : lexer xs
lexer (' ':xs)  = lexer xs
lexer ('\n':xs) = lexer xs
lexer ('\r':xs) = lexer xs
lexer ('\t':xs) = lexer xs
lexer xs = [Err (take 10 xs)]

prec :: BOps -> Integer
prec AddOp = 10
prec MulOp = 20
prec SubOp = 10
prec DivOp = 20
prec ModOp = 20
prec ExpOp = 30
prec AndOp = 5
prec OrOp = 1
prec EqOp = 0
prec LtOp = 0
prec LteOp = 0
prec GtOp = 0
prec GteOp = 0

sr :: [Token] -> [Token] -> [Token]
sr (Semi : PA e : AssignOp : (PA(Var v)) : xs) i = sr (PI (Assign v e) : xs) i
sr (VSym v : xs) i = sr (PA (Var v) : xs) i
sr (CSym x : xs) i = sr (PA (Num x) : xs) i
sr (BSym x : xs) i = sr (PB (Const x) : xs) i
sr (RPar : PA e : LPar : xs) i = sr (PA e : xs) i
sr (RPar : PB e : LPar : xs) i = sr (PB e : xs) i
sr (LBra : xs) i = sr (Block [] : xs) i
sr (PI y : Block zs : xs) i = sr (Block (y : zs) : xs) i
sr (RBra : Block zs : xs) i = sr (PI (Do (reverse zs)) : xs) i
sr (PB x : NotOp : xs) i = sr (PB (Not x) : xs) i
sr stack@( _ : BOp op : _ : s) (BOp op' : i) | prec op < prec op' = sr (BOp op' : stack) i
sr (PA y : BOp LteOp : PA x : xs) i = sr (PB (Lte x y) : xs) i
sr (PA y : BOp LtOp : PA x : xs) i = sr (PB (Lt x y) : xs) i 
sr (PA y : BOp GteOp : PA x : xs) i = sr (PB (Gte x y) : xs) i 
sr (PA y : BOp GtOp : PA x : xs) i = sr (PB (Gt x y) : xs) i
sr (PA y : BOp NeqOp : PA x : xs) i = sr (PB (Neq x y) : xs) i 
sr (PA y : BOp EqOp : PA x : xs) i = sr (PB (Eq x y) : xs) i
sr (PB y : BOp OrOp : PB x : xs) i = sr (PB (Or x y) : xs) i
sr (PB y : BOp AndOp : PB x : xs) i = sr (PB (And x y) : xs) i
sr (PA y : BOp ExpOp : PA x : xs) i = sr (PA (Exp x y) : xs) i
sr (PA y : BOp ModOp : PA x : xs) i = sr (PA (Mod x y) : xs) i
sr (PA y : BOp MulOp : PA x : xs) i = sr (PA (Mul x y) : xs) i
sr (PA y : BOp DivOp : PA x : xs) i = sr (PA (Div x y) : xs) i
sr (PA y : BOp AddOp : PA x : xs) i = sr (PA (Add x y) : xs) i
sr (PA y : BOp SubOp : PA x : xs) i = sr (PA (Sub x y) : xs) i
sr (PI y : Keyword ElseK : PI x : Keyword ThenK : PB b : Keyword IfK : xs) i = sr (PI (IfThenElse b x y) : xs) i
sr (PI x : Keyword ThenK : PB b : Keyword IfK : xs) i = sr (PI (IfThen b x) : xs) i
sr (PI x : PB b : Keyword WhileK : xs) i = sr (PI (While b x) : xs) i
sr (Semi : Keyword NopK : xs) i = sr (PI (Nop) : xs) i
sr (Semi : PA a : Keyword ReturnK : xs) i = sr (PI (Return a) : xs) i
sr (Err e : xs) i = [Err e]
sr s (i : is) = sr (i : s) is
sr s [] = s

update :: (Vars, Integer) -> Env -> Env
update (var, num) [] = [(var, num)]
update (var, num) ((x, y):env)
    | (var == x) = (var, num) : env
    | otherwise = (x, y) : update (var, num) env

readProg :: [Token] -> Either [Instr] String
readProg t = case sr [] ([LBra] ++ t ++ [RBra]) of
                 [PI (Do e)] -> Left e
                 [Err e] -> Right $ "Lexical error: " ++ e
                 s       -> Right $ "Parse error: " ++ show s

env1 :: Env
env1 = [("sum",10), ("y",3), ("i",5), ("acc",1),("c",3),("n",2)]
env2 :: Env
env2 = [("sum",20), ("y",100), ("i",5), ("acc",10),("c",4),("n",5)]

evala :: Env -> AExpr -> Integer
evala e (Var x) = case lookup x e of
     Just x -> x
     Nothing -> error ("Variable not found: " ++ x)
evala e (Num x) = x
evala e (Exp x y) = evala e x ^ evala e y
evala e (Mul x y) = evala e x * evala e y
evala e (Mod x y) = evala e x `mod` evala e y
evala e (Div x y) = evala e x `div` evala e y
evala e (Add x y) = evala e x + evala e y
evala e (Sub x y) = evala e x - evala e y

evalb :: Env -> BExpr -> Bool
evalb e (Const b) = b
evalb e (Not x) = not (evalb e x)
evalb e (And x y) = evalb e x && evalb e y
evalb e (Or x y) = evalb e x || evalb e y
evalb e (Eq x y) = evala e x == evala e y
evalb e (Neq x y) = evala e x /= evala e y
evalb e (Lt x y) = evala e x < evala e y
evalb e (Lte x y) = evala e x <= evala e y
evalb e (Gt x y) = evala e x > evala e y
evalb e (Gte x y) = evala e x >= evala e y

exec :: Instr -> Env -> Env
exec (Assign var aExp) env = update (var, evala env aExp) env
exec (IfThen bExp ins) env = if evalb env bExp then exec ins env else env
exec (IfThenElse bExp i1 i2) env = if evalb env bExp then exec i1 env else exec i2 env
exec (While bExp ins) env = if evalb env bExp then exec (While bExp ins) (exec ins env) else env
exec (Do ins) env = execList ins env
exec Nop env = env
exec (Return aExp) env = [("", evala env aExp)]

execList :: [Instr] -> Env -> Env
execList [] e = e
execList (x : xs) e = if ((lookup "" e) == Nothing) then execList xs (exec x e) else e 
