import Data.List (isPrefixOf)
import Data.Char

type Vars  = String
type Value = Double
type Env = [(Vars,Value)]

data AExpr = Var Vars | Num Value
           | Add AExpr AExpr | Sub AExpr AExpr
           | Mul AExpr AExpr | Div AExpr AExpr
           | FCall String [AExpr]
  deriving (Show,Eq)

data BOps = AddOp | MulOp | SubOp | DivOp
  deriving (Show,Eq)
data Token = VSym Vars | CSym Value | BOp BOps
           | LPar | RPar | Err String | PA AExpr
           | QuitT | VarsT | LoadT String | AssignOp
  deriving (Show,Eq)

lexer :: String -> [Token]
lexer "" = []
lexer cs | isPrefixOf ":quit" cs = [QuitT] -- : lexer (drop 5 cs )
lexer cs | isPrefixOf ":vars" cs = [VarsT] -- : lexer (drop 5 cs )
lexer cs | isPrefixOf ":load" cs = [LoadT (dropWhile isSpace (drop 5 cs))]
lexer (':':'=':cs) = AssignOp : lexer cs
lexer ('+':cs) = BOp AddOp : lexer cs
lexer ('-':cs) = BOp SubOp : lexer cs
lexer ('*':cs) = BOp MulOp : lexer cs
lexer ('/':cs) = BOp DivOp : lexer cs
lexer ('(':cs) = LPar : lexer cs
lexer (')':cs) = RPar : lexer cs
--Same thing but checking for upper
lexer (c:cs) | isUpper c = NSym var : lexer rest
  where (var,rest) = span isAlphaNum (c:cs)

-- variables
lexer (c:cs) | isLower c = VSym var : lexer rest
  where (var,rest) = span isAlphaNum (c:cs)
-- constants
lexer (c:cs) | isDigit c = CSym (read num) : lexer rest
  where q1 (x:xs) | isDigit x = let (c,d) = q1 xs in (x:c,d)
        q1 ('.':x:xs) | isDigit x = let (c,d) = q3 (x:xs) in ('.':c,d)
        q1 xs = ("",xs)
        q3 xs = span isDigit xs
        (num,rest) = q1 (c:cs)
  -- whitespace skipping, can be improved by importing isSpace from Data.Char
lexer (c:cs) | isSpace c = lexer cs
-- unrecognized symbol error, with a 10-character context window
lexer cs = [Err (take 10 cs)]

prec :: BOps -> Integer
prec AddOp = 10
prec SubOp = 10
prec MulOp = 20
prec DivOp = 20

-- sr :: Stack -> Queue -> "Final state of the stack"
sr :: [Token] -> [Token] -> [Token]
-- Grammar rules: Reduce phase
sr (VSym v : s) q = sr (PA (Var v) : s) q
sr (CSym c : s) q = sr (PA (Num c) : s) q
sr s@(PA _ : BOp op1 : PA _ : _) (BOp op2 : q) | prec op2 > prec op1
  = sr (BOp op2 : s) q
sr (PA p2 : BOp AddOp : PA p1 : s) q = sr (PA (Add p1 p2) : s) q
sr (PA p2 : BOp SubOp : PA p1 : s) q = sr (PA (Sub p1 p2) : s) q
sr (PA p2 : BOp MulOp : PA p1 : s) q = sr (PA (Mul p1 p2) : s) q
sr (PA p2 : BOp DivOp : PA p1 : s) q = sr (PA (Div p1 p2) : s) q
sr (RPar : PA p : LPar : s) q = sr (PA p : s) q
-- Moving from input to the stack: Shift phase
sr s [Err e] = [Err e]
sr s (q:qs) = sr (q:s) qs
-- Base case
sr s [] = s

eval :: Env -> AExpr -> Value
-- eval env (Var x) = lookUp x env
eval env (Var x) = case (lookup x env) of
                     Just n  -> n
                     Nothing -> error $ "Variable not found: " ++ x
eval env (Num n) = n
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Div e1 e2) = eval env e1 / eval env e2
eval env (FCall f args) = 
  let argsvalues = map (eval env) args
  {-
    1. Chapter 8 -- Of the Synt
    2. Parsing Lists

    Parsing just means making numerical values into key words.
    Wanting to implement something that does this: 
    F x y = x^2 + y
    G x = F(x, F(1,2*x))
    > G 5 = F (5,F(1,2*5))
          = F (5,1^2+2*5) = F(5,11)
                          = 25 + 11 = 36

We basically want to assign f, x, aand y to those numbers)


-}

parseAExpr :: [Token] -> Either AExpr String
parseAExpr s = case sr [] s of
  [PA  e] -> Left e
  [Err e] -> Right $ "Lexical error: " ++ e
  s       -> Right $ "Parse error: " ++ show s

update :: (Vars,Value) -> Env -> Env
update (x,n) [] = [(x,n)]
update (x,n) ((y,m):es) = if x==y then (y,n):es else (y,m) : update (x,n) es

main = repl []

updateEnv :: Env -> [Token] -> Env
updateEnv env (VSym x : AssignOp : ts) = case parseAExpr ts of
  Left e  -> update (x,eval env e) env
  Right e -> error e

-- Read-Eval-Print-Loop
repl :: Env -> IO ()
repl env = do
  s <- getLine
  case (lexer s) of
    [QuitT] -> return ()
    [VarsT] -> putStrLn (show env) >> repl env
    [LoadT filename] -> do
      contents <- readFile filename
      let fileLines = lines contents
      let newenv = foldl updateEnv [] (map lexer fileLines)
      repl newenv
    (VSym x : AssignOp : ts) ->
      case (parseAExpr ts) of
        (Left e)  -> repl (update (x,eval env e) env)
        (Right e) -> putStrLn e >> repl env
    ts -> case (parseAExpr ts) of
            (Left e)  -> putStrLn (show (eval env e))
            (Right e) -> putStrLn e
          >> repl env



















-- Examples of working with the IO type

-- get input from the user, reverse it, and then print it twice
example :: IO ()
example = getLine >>= f
  where f s = putStrLn (reverse s) >>= (\x -> putStrLn (reverse s))

-- get a string, output the length of it
example2 :: IO ()
example2 = getLine >>= f
  where f s = putStrLn (show (length s))

-- input a string, s1
-- input another string, s2
-- output concatenation s2++s1, and the length of the final string
example3 :: IO ()
example3 =
  getLine >>= (\s1 ->
    getLine >>= (\s2 ->
      putStrLn (s2 ++ s1) >>= (\x ->
        putStrLn ("length is: " ++ show (length (s2++s1))))))

-- improved version of the above
example4 :: IO ()
example4 = do
  s1 <- getLine
  s2 <- getLine
  putStrLn (s2 ++ s1)
  putStrLn (show (length (s2 ++ s1)))

fact :: Integer -> Integer
fact n = product [1..n]
