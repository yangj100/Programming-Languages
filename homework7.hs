import Data.List
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Debug.Trace (trace)

type Vars = String
data Prop = Var Vars | Const Bool | And Prop Prop | Or Prop Prop | Not Prop | Imp Prop Prop | Iff Prop Prop | Xor Prop Prop deriving (Show,Eq)

---------------------HW6-----------------------

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
varsInProp (Imp p q)     = nub (varsInProp p ++ varsInProp q)
varsInProp (Iff p q)     = nub (varsInProp p ++ varsInProp q)
varsInProp (Xor p q)     = nub (varsInProp p ++ varsInProp q)
sat :: Prop -> Bool
sat p = evalList p (genEnvs(varsInProp p))

-- checkEq :: Prop -> Prop -> Bool
-- checkEq f1 f2 = 
--     let vars = nub (varsInProp f1 ++ varsInProp f2)
--         envs = genEnvs vars 
--     in all (\env -> eval env f1 == eval env f2) envs 



-------END OF HW6------------------------------------
--2.1
fv :: Prop -> [Vars]
fv (Var x)      = [x]
fv (Not p)      = fv p
fv (And p q)    = fv p `union` fv q
fv (Or p q)     = fv p `union` fv q
fv (Const _)    = []
fv (Imp p q)    = fv p `union` fv q
fv (Iff p q)    = fv p `union` fv q
fv (Xor p q)    = fv p `union` fv q

--2.2

type Value = Bool
type Env = [(Vars, Value)]
eval :: Env -> Prop -> Value
eval env (Var y)    = lookUp y env 
eval env (Not p)    = not (eval env p)
eval env (And p q)  = eval env p && eval env q
eval env (Or p q)   = eval env p || eval env q
eval _ (Const b)    = b
eval env (Imp p q)  = not (eval env p) || eval env q
eval env (Iff p q)  = eval env p == eval env q
eval env (Xor p q)  = eval env p /= eval env q 

--3.1.1
contra :: Prop -> Bool
contra p    = not (sat p)
--3.1.2
tauto :: Prop -> Bool
tauto p    = contra (Not p)

findSat :: Prop -> Maybe Env
findSat p = find (`eval` p) (genEnvs (varsInProp p))


findRefute :: Prop -> Maybe Env
findRefute p = 
    let vars = varsInProp p  
        envs = genEnvs vars
    in find (\env -> not (eval env p)) envs

classify :: Prop -> String
classify p = 
  let envs = genEnvs (varsInProp p)          
      evalResults = map (\env -> eval env p) envs 
  in
      if all id evalResults then "tautology" 
      else if none id evalResults then "contradiction" 
      else "contingency"             

none :: (a -> Bool) -> [a] -> Bool
none p = not . any p

checkEq :: Prop -> Prop -> Bool
checkEq p1 p2 = 
  let vars1 = varsInProp p1
      vars2 = varsInProp p2
      envs = genEnvs (vars1 `union` vars2)
      evalP1 = map (\env -> eval env p1) envs
      evalP2 = map (\env -> eval env p2) envs
  in evalP1 == evalP2

refuteEq :: Prop -> Prop -> Maybe Env
refuteEq p1 p2 = 
  let vars1 = varsInProp p1
      vars2 = varsInProp p2
      envs = genEnvs (vars1 `union` vars2)
      differingEnv = find (\env -> eval env p1 /= eval env p2) envs
  in differingEnv

data BOps = AndOp | OrOp | ImpOp | IffOp | XorOp
  deriving (Show, Eq)

data Token = VSym String | CSym Bool | BOp BOps | NotOp | LPar | RPar
           | Err String | PB Prop
  deriving (Show, Eq)
  
lexer :: String -> [Token]
lexer [] = []
lexer (' ':s) = lexer s 
lexer ('!':s) = NotOp : lexer s
lexer ('/':'\\':s) = BOp AndOp : lexer s  -- AndOp ("/\\")
lexer ('\\':'/':s) = BOp OrOp : lexer s  -- OrOp ("\\/")
lexer ('<':'-':'>':s) = BOp IffOp : lexer s  -- IffOp ("<->")
lexer ('<':'+':'>':s) = BOp XorOp : lexer s  -- XorOp ("<+>")
lexer ('-':'>':s) = BOp ImpOp : lexer s  -- ImpOp ("->")
lexer ('t':'t':s) = CSym True : lexer s
lexer ('t':'r':'u':'e':s) = CSym True : lexer s
lexer ('f':'f':s) = CSym False : lexer s
lexer ('f':'a':'l':'s':'e':s) = CSym False : lexer s
lexer ('(':s) = LPar : lexer s 
lexer (')':s) = RPar : lexer s 
lexer (c:s)
  | isUpper c = let (var, rest) = span isAlphaNum (c:s)
                in VSym var : lexer rest 
lexer (c:s) = Err [c] : lexer s 

prec :: BOps -> Integer
prec AndOp = 20
prec OrOp = 10

sr :: [Token] -> [Token] -> [Token]

sr (VSym v : s) q = sr (PB (Var v) : s) q
sr (CSym c : s) q = sr (PB (Const c) : s) q


sr (NotOp : PB p : s) q = sr (PB (Not p) : s) q
sr (NotOp : NotOp : PB p : s) q = sr (PB (Not (Not p)) : s) q

sr (PB p2 : BOp AndOp : PB p1 : s) q = sr (PB (And p1 p2) : s) q
sr (PB p2 : BOp OrOp : PB p1 : s) q = sr (PB (Or p1 p2) : s) q
sr (PB p2 : BOp ImpOp : PB p1 : s) q = sr (PB (Imp p1 p2) : s) q
sr (PB p2 : BOp IffOp : PB p1 : s) q = sr (PB (Iff p1 p2) : s) q
sr (PB p2 : BOp XorOp : PB p1 : s) q = sr (PB (Xor p1 p2) : s) q

sr (LPar : s) (PB p : RPar : q) = sr (PB p : s) q
sr (RPar : PB p : LPar : s) q = sr (PB p : s) q


sr s [Err e] = [Err e]
sr s (q:qs) = sr (q:s) qs

sr s [] = s


parseProp :: String -> Either Prop String
parseProp s = case lexer s of
  [] -> Right "Lexical error: Empty input."
  tokens -> case sr [] tokens of
    [PB e] -> Left e 
    [Err e] -> Right $ "Lexical error: " ++ e
    _ -> Right $ "Parse error: " ++ show tokens


checkLexicalErrors :: [Token] -> Maybe String
checkLexicalErrors [] = Nothing
checkLexicalErrors (Err msg:_) = Just ("Lexical error: " ++ msg)  
checkLexicalErrors (_:ts) = checkLexicalErrors ts 

main :: IO ()
main = do
  putStrLn "Enter a boolean formula:"
  loop

loop :: IO ()
loop = do
  input <- getLine
  if input == "quit" then return ()
  else do
    let result = parseProp input
    case result of
      Left prop -> do
        putStrLn $ "Parsed successfully: " ++ show prop
      Right err -> putStrLn err
    loop
