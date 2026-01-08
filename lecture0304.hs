import Data.List (isPrefixOf)

-- The string obtained by reading C source code of a function to
-- add up the squares of all numbers from 1 to n.

example :: String
example = "int addSquares (int n)\n {int i;\nint sum=0;\nfor (i=0; i<=n; i++)\n{\nsum = sum + i*i;\n}\nreturn sum;\n }\n"

-- Our goal: transform the string above into a list of Tokens

output :: [Token]
output = [Type IntType, VSym "addSquares", LPar, Type IntType,
  VSym "n", RPar, LBra, Type IntType, VSym "i", Semi,
  Type IntType, VSym "sum", EqSym, CSym 0, Semi, Keyword ForK,
  LPar, VSym "i", EqSym, CSym 0, Semi, VSym "i", BOp LteOp,
  VSym "n", Semi, VSym "i", UOp IncrOp, RPar, LBra, VSym "sum",
  EqSym, VSym "sum", BOp AddOp, VSym "i", BOp MulOp, VSym "i",
  Semi, RBra, Keyword ReturnK, VSym "sum", Semi, RBra]


-- Definition of token datatypes

data BOps = AddOp | SubOp | MulOp | DivOp | LtOp | LteOp
          | AndOp | OrOp  | EqlOp
  deriving (Show,Eq)
data UOps = IncrOp | DecrOp | NotOp
  deriving (Show,Eq)
data Keywords = ForK | ReturnK | WhileK | IfK
  deriving (Show,Eq)
data Types    = IntType | BoolType | FloatType
  deriving (Show,Eq)
data Token = CSym Integer | VSym String | BOp BOps | EqSym
           | Keyword Keywords | LPar | RPar | LBra | RBra
           | Semi | Err String | Type Types | UOp UOps
  deriving (Show,Eq)

-- These functions can be replaced with putting "import Data.Char" at the top of the file
isLower :: Char -> Bool
isLower c = elem c ['a'..'z']
isUpper :: Char -> Bool
isUpper c = elem c ['A'..'Z']
isDigit :: Char -> Bool
isDigit c = elem c ['0'..'9']
isAlphaNum :: Char -> Bool
isAlphaNum c = isLower c || isUpper c || isDigit c

-- Lexical analyser transforms the input string into a list of tokens
lexer :: String -> [Token]
lexer "" = []
-- keywords
lexer cs | isPrefixOf "for" cs = Keyword ForK : lexer (drop 3 cs)
lexer cs | isPrefixOf "return" cs = Keyword ReturnK : lexer (drop 6 cs)
-- types
lexer ('i':'n':'t':cs)     = Type IntType : lexer cs
lexer ('b':'o':'o':'l':cs) = Type BoolType : lexer cs
-- variables
lexer (c:cs) | isLower c = VSym v : lexer rest
  where (v,rest) = span isAlphaNum (c:cs)
-- constants
lexer (c:cs) | isDigit c = CSym (read n) : lexer rest
  where (n,rest) = span isDigit (c:cs)
-- operators
lexer ('+':'+':cs) = UOp IncrOp : lexer cs
lexer ('-':'-':cs) = UOp DecrOp : lexer cs
lexer ('+':cs) = BOp AddOp : lexer cs
lexer ('-':cs) = BOp SubOp : lexer cs
lexer ('*':cs) = BOp MulOp : lexer cs
lexer ('/':cs) = BOp DivOp : lexer cs
lexer ('=':'=':cs) = BOp EqlOp : lexer cs
lexer ('<':'=':cs) = BOp LteOp : lexer cs
lexer ('<':cs)     = BOp LtOp : lexer cs
-- punctuation
lexer ('(':cs) = LPar : lexer cs
lexer (')':cs) = RPar : lexer cs
lexer ('{':cs) = LBra : lexer cs
lexer ('}':cs) = RBra : lexer cs
lexer (';':cs) = Semi : lexer cs
lexer ('=':cs) = EqSym : lexer cs
-- whitespace skipping, can be improved by importing isSpace from Data.Char
lexer (c:cs) | elem c " \t\n\r" = lexer cs
-- unrecognized symbol error, with a 10-character context window
lexer cs = [Err (take 10 cs)]

-- testing whether the function matches the expected output 
test :: Bool
test = lexer example == output
