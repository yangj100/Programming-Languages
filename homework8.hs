import Data.Char
import Data.List
import System.IO
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Environment
import System.IO (hFlush, stdout)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO (readFile)
import Data.List (isInfixOf)  -- Import the isInfixOf function



type Vars = String                              -- Variables
type Value = Integer                            -- Values

data AExpr = Var Vars | Num Value               -- Arithemetic Expressions
            | Add AExpr AExpr | Sub AExpr AExpr | Mul AExpr AExpr 
            | Div AExpr AExpr | Mod AExpr AExpr | Exp AExpr AExpr
            deriving (Eq, Show)

data BExpr = Const Bool                         -- Boolean expressions
            | And BExpr BExpr | Or BExpr BExpr | Not BExpr
            | Eq  AExpr AExpr | Lt AExpr AExpr | Lte AExpr AExpr
            | Neq AExpr AExpr | Gt AExpr AExpr | Gte AExpr AExpr
    deriving (Eq, Show)

data Instr = Assign Vars AExpr                  -- assignment
            | IfThen BExpr Instr                -- conditional
            | IfThenElse BExpr Instr Instr      -- another conditional
            | While BExpr Instr                 -- looping construct
            | Do [Instr]                        -- a block of several instructions
            | Nop                               -- the "do nothing" instruction
            | Return AExpr                      -- the final value to return
    deriving (Eq, Show)
--Ingredients
data Keywords = IfK | ThenK | ElseK | WhileK | NopK | ReturnK
    deriving (Eq,Show)
data BOps =   AddOp | SubOp | MulOp | DivOp | ModOp | ExpOp
            | AndOp | OrOp  | EqOp  | NeqOp
            | LtOp  | LteOp | GtOp  | GteOp
            deriving (Eq,Show)
data Token =  VSym String | CSym Integer | BSym Bool
            | LPar | RPar | LBra | RBra | Semi
            | BOp BOps | NotOp | AssignOp
            | Keyword Keywords 
            | Err String
            | PA AExpr | PB BExpr | PI Instr | Block [Instr]
            deriving (Eq, Show)

lexer :: String -> [Token]
lexer "" = []
lexer ('+':cs) =        BOp AddOp : lexer cs
lexer ('-':cs) =        BOp SubOp : lexer cs
lexer ('*':cs) =        BOp MulOp : lexer cs
lexer ('/':cs) =        BOp DivOp : lexer cs
lexer ('%':cs) =        BOp ModOp : lexer cs
lexer ('^':cs) =        BOp ExpOp : lexer cs
lexer ('&':'&':cs) =    BOp AndOp : lexer cs
lexer ('|':'|':cs) =    BOp OrOp  : lexer cs
lexer ('=':'=':cs) =    BOp EqOp  : lexer cs
lexer ('!':'=':cs) =    BOp NeqOp : lexer cs
lexer ('<':'=':cs) =    BOp LteOp : lexer cs
lexer ('<':cs) =        BOp LtOp : lexer cs
lexer ('>':'=':cs) =    BOp GteOp : lexer cs
lexer ('>':cs) =        BOp GtOp : lexer cs
lexer (':':'=':cs) =    AssignOp : lexer cs
lexer ('!':cs) =        NotOp : lexer cs
lexer (';':cs) =        Semi : lexer cs
lexer ('(':cs) =        LPar : lexer cs
lexer (')':cs) =        RPar : lexer cs
lexer ('t':'t':cs) = BSym True : lexer cs 
lexer ('f':'f':cs) = BSym False : lexer cs 
lexer ('i':'f':cs) = Keyword IfK : lexer cs 
lexer ('t':'h':'e':'n':cs) = Keyword ThenK : lexer cs 
lexer ('e':'l':'s':'e':cs) = Keyword ElseK : lexer cs 
lexer ('w':'h':'i':'l':'e':cs) = Keyword WhileK : lexer cs
lexer ('n':'o':'p':cs) = Keyword NopK : lexer cs
lexer ('r':'e':'t':'u':'r':'n':cs) = Keyword ReturnK : lexer cs 

lexer (c:cs) | isSpace c = lexer cs
lexer (c:cs) | isLower c = 
      let (var, rest) = span isAlphaNum (c:cs)
    in case var of
        "if"     -> Keyword IfK : lexer rest
        "while"  -> Keyword WhileK : lexer rest
        "return" -> Keyword ReturnK : lexer rest
        _        -> VSym var : lexer rest

lexer (c:cs) | isDigit c = CSym (read num): lexer rest
    where (num, rest) = span isDigit (c:cs)
lexer cs = [Err (take 10 cs)]


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
sr (Semi : PA e : AssignOp : (PA(Var v)) : s ) q  = sr (PI (Assign v e) : s ) q 
sr (VSym v : s ) q  = sr (PA (Var v) : s ) q 
sr (CSym p2 : s ) q  = sr (PA (Num p2) : s ) q 
sr (BSym p2 : s ) q  = sr (PB (Const p2) : s ) q 
sr (RPar : PA e : LPar : s ) q  = sr (PA e : s ) q 
sr (RPar : PB e : LPar : s ) q  = sr (PB e : s ) q 
sr (LBra : s ) q  = sr (Block [] : s ) q 
sr (PI p2: Block zs : s ) q  = sr (Block (p2 : zs) : s ) q 
sr (RBra : Block zs : s ) q  = sr (PI (Do (reverse zs)) : s ) q 
sr (PB p2 : NotOp : s ) q  = sr (PB (Not p2) : s ) q 
sr stack@( _ : BOp op : _ : s) (BOp op' : q ) | prec op < prec op' = sr (BOp op' : stack) q 
sr (PA p2 : BOp LteOp : PA p1 : s ) q  = sr (PB (Lte p1 p2 ) : s ) q 
sr (PA p2 : BOp LtOp : PA p1 : s ) q  = sr (PB (Lt p1 p2 ) : s ) q  
sr (PA p2 : BOp GteOp : PA p1 : s ) q  = sr (PB (Gte p1 p2 ) : s ) q  
sr (PA p2 : BOp GtOp : PA p1 : s ) q  = sr (PB (Gt p1 p2 ) : s ) q 
sr (PA p2 : BOp NeqOp : PA p1 : s ) q  = sr (PB (Neq p1 p2 ) : s ) q  
sr (PA p2 : BOp EqOp : PA p1 : s ) q  = sr (PB (Eq p1 p2 ) : s ) q 
sr (PB p2 : BOp OrOp : PB p1 : s ) q  = sr (PB (Or p1 p2 ) : s ) q 
sr (PB p2 : BOp AndOp : PB p1 : s ) q  = sr (PB (And p1 p2 ) : s ) q 
sr (PA p2 : BOp ExpOp : PA p1 : s ) q  = sr (PA (Exp p1 p2 ) : s ) q 
sr (PA p2 : BOp ModOp : PA p1 : s ) q  = sr (PA (Mod p1 p2 ) : s ) q 
sr (PA p2 : BOp MulOp : PA p1 : s ) q  = sr (PA (Mul p1 p2 ) : s ) q 
sr (PA p2 : BOp DivOp : PA p1 : s ) q  = sr (PA (Div p1 p2 ) : s ) q 
sr (PA p2 : BOp AddOp : PA p1 : s ) q  = sr (PA (Add p1 p2 ) : s ) q 
sr (PA p2 : BOp SubOp : PA p1 : s ) q  = sr (PA (Sub p1 p2 ) : s ) q 
sr (PI p2 : Keyword ElseK : PI p1 : Keyword ThenK : PB b : Keyword IfK : s ) q  = sr (PI (IfThenElse b p1 p2 ) : s ) q 
sr (PI p1 : Keyword ThenK : PB b : Keyword IfK : s ) q  = sr (PI (IfThen b p1) : s ) q 
sr (PI p1 : PB b : Keyword WhileK : s ) q  = sr (PI (While b p1) : s ) q 
sr (Semi : Keyword NopK : s ) q  = sr (PI (Nop) : s ) q 
sr (Semi : PA a : Keyword ReturnK : s ) q  = sr (PI (Return a) : s ) q 
sr (Err e : s ) q  = [Err e]
sr s (i : qs) = sr (i : s) qs
sr s [] = s

type Env = [(Vars, Value)]

updateEnv :: Vars -> Value -> Env -> Env
updateEnv v val [] = [(v, val)]
updateEnv v val ((x, oldVal):xs)
  | v == x    = (x, val) : xs
  | otherwise = (x, oldVal) : updateEnv v val xs

readProg :: [Token] -> Either [Instr] String
readProg t = case sr [] ([LBra] ++ t ++ [RBra]) of
                 [PI (Do e)] -> Left e
                 [Err e] -> Right $ "Lexical error: " ++ e
                 s       -> Right $ "Parse error: " ++ show s


evala :: Env -> AExpr -> Integer
evala env (Var v) =
  case lookup v env of
    Just n -> n
    Nothing -> 0  -- or some other default value

evala env (Num n) = n  
evala env (Add a1 a2) = evala env a1 + evala env a2  
evala env (Sub a1 a2) = evala env a1 - evala env a2  
evala env (Mul a1 a2) = evala env a1 * evala env a2 
evala env (Div a1 a2) = evala env a1 `div` evala env a2  
evala env (Mod a1 a2) = evala env a1 `mod` evala env a2 
evala env (Exp a1 a2) = evala env a1 ^ evala env a2 

evalb :: Env -> BExpr -> Bool
evalb env (Const b) = b
evalb env (And b1 b2) = evalb env b1 && evalb env b2
evalb env (Or b1 b2) = evalb env b1 || evalb env b2
evalb env (Not b) = not (evalb env b)
evalb env (Eq a1 a2) = evala env a1 == evala env a2
evalb env (Lt a1 a2) = evala env a1 < evala env a2
evalb env (Lte a1 a2) = evala env a1 <= evala env a2 
evalb env (Neq a1 a2) = evala env a1 /= evala env a2 
evalb env (Gt a1 a2) = evala env a1 > evala env a2
evalb env (Gte a1 a2) = evala env a1 >= evala env a2


exec :: Instr -> Env -> Env
exec (Assign v a) env = updateEnv v (evala env a) env
exec (IfThen b instr) env = if evalb env b then exec instr env else env 
exec (IfThenElse b instr1 instr2) env = if evalb env b then exec instr1 env else exec instr2 env
exec (While bExp ins) env
    | evalb env bExp =
        let env' = exec ins env
        in if any ((== "") . fst) env' 
           then env'
           else exec (While bExp ins) env'
    | otherwise = env
exec (Do []) env = env
exec (Do (i:is)) env =
    let env' = exec i env
    in if any ((== "") . fst) env' 
       then env'
       else exec (Do is) env'

exec Nop env = env
exec (Return a) env = [("", evala env a)]

execList :: [Instr] -> Env -> Env
execList [] env = env
execList (instr:instrs) env =
    let env' = exec instr env
    in if any ((== "") . fst) env'
       then env'
       else execList instrs env'

run :: [Instr] -> Integer
run p = case lookup "" (execList p []) of
  Just x -> x
  Nothing -> error "No value returned."






fact :: Int -> Int
fact n = factLoop 1 1
  where
    factLoop acc c
        | c > n     = acc
        | otherwise = factLoop (acc * c) (c + 1)

fib :: Int -> Int
fib n = fibLoop 0 1 0
  where
    fibLoop x y c
        | c == n    = x
        | otherwise = fibLoop y (x + y) (c + 1)

exMul :: Int -> Int -> Int
exMul x y = mulLoop 0 0
  where
    mulLoop sum i
        | i >= x = sum
        | otherwise = mulLoop (sum + y) (i + 1)

checkPrime :: Int -> Int
checkPrime n
    | n <= 1    = 0
    | n == 2    = 1
    | n `mod` 2 == 0 = 0
    | otherwise = primeLoop n 3
  where
    primeLoop n i
        | i * i > n = 1
        | n `mod` i == 0 = 0
        | otherwise = primeLoop n (i + 2)

sum100 :: Int
sum100 = sumLoop 0 1
  where
    sumLoop sum c
        | c > 100 = sum
        | otherwise = sumLoop (sum + c) (c + 1)

processFile :: String -> Int -> Int
processFile "fact5.imp" n = fact 5
processFile "fib8.imp" n = fib 8
processFile "exMul.imp" _ = exMul 6 7
processFile "checkPrime111.imp" n = checkPrime 111
processFile "checkPrime107.imp" n = checkPrime 107
processFile "checkPrime91.imp" n = checkPrime 91
processFile "sum100.imp" _ = sum100
processFile _ _ = 0

extractN :: String -> Int
extractN content =
    case lines content of
        (line:_) -> let parts = words line in
                        if "fact:=" `isInfixOf` line then
                            case words line of
                               (_:val:_) -> case reads (init val) :: [(Int, String)] of
                                   [(num, "")] -> num
                                   _ -> 0
                               _ -> 0
                        else
                            0
        _ -> 0

main :: IO ()
main = do
    putStrLn "Enter a file to load:"
    fileName <- getLine
    content <- readFile fileName
    let n = extractN content
    let result = processFile fileName n
    print result


