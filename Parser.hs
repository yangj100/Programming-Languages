-- Parser.hs
module Parser (parseFromFile) where

import Text.Parsec
import Text.Parsec.String (Parser)

-- Define the Instr and Expr types
data Expr
    = Num Integer
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Lt Expr Expr
    | Eql Expr Expr
    | Not Expr
    deriving (Show)

data Instr
    = Assign String Expr
    | IfThenElse Expr Instr Instr
    | While Expr Instr
    | Do [Instr]
    | Return Expr
    | Nop
    deriving (Show)

-- Parsing functions (you may need to modify this based on your full grammar)
parseNum :: Parser Expr
parseNum = do
    n <- many1 digit
    return $ Num (read n)

parseVar :: Parser Expr
parseVar = do
    var <- many1 letter
    return $ Var var

parseAssign :: Parser Instr
parseAssign = do
    _ <- string "Assign"
    _ <- spaces
    var <- many1 letter
    _ <- spaces
    _ <- char '='
    _ <- spaces
    expr <- parseVar
    return $ Assign var expr

parseProgram :: Parser [Instr]
parseProgram = many parseAssign  -- For simplicity, only parsing Assign instructions

-- Parse from file function
parseFromFile :: FilePath -> IO (Either ParseError [Instr])
parseFromFile fileName = do
    content <- readFile fileName
    return $ parse parseProgram "" content
