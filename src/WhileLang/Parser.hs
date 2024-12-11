{-
 - Disclaimer: Quite a bit of this code comes from the megaparsec tutorial found here: 
 - https://markkarpov.com/tutorial/megaparsec.html
  -}

{-# LANGUAGE OverloadedStrings #-}

module WhileLang.Parser where

import WhileLang.Syntax

import Data.Text (Text)
import Data.Void
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- Space Consumer
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reserved :: [String]
reserved = [
            "true"
          , "false"
          , "not"
          , "and"
          , "or"
          , "skip"
          , "if"
          , "then"
          , "else"
          , "fi"
          , "while"
          , "do"
          , "od"
          ]

-- Operator Boilerplate
binary :: Text -> (a -> a -> a) -> Operator Parser a
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (a -> a) -> Operator Parser a 
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

-- Arithmetic Expressions
pVariable :: Parser ArithExpr
pVariable = do
  name <- lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")
  if name `elem` reserved 
    then fail $ "keyword " ++ show name ++ " cannot be used as a variable"
    else return (Var name)

pInteger :: Parser ArithExpr
pInteger =  Num <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pArithTerm :: Parser ArithExpr
pArithTerm = choice
  [ pVariable
  , pInteger
  , parens pArithExpr
  ]

arithOpTable :: [[Operator Parser ArithExpr]]
arithOpTable = 
  [ [ prefix "-" Neg
    , prefix "+" id
    ]
  , [ binary "*" Mul
    , binary "/" Div
    ]
  , [ binary "+" Add
    , binary "-" Sub
    ]
  ]

pArithExpr :: Parser ArithExpr
pArithExpr = makeExprParser pArithTerm arithOpTable

-- Boolean Expression Parser
pBoolLiteral :: Parser BoolExpr
pBoolLiteral = BoolLiteral <$> lexeme (True <$ symbol "true" <|> False <$ symbol "false")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

pComparison :: Parser BoolExpr
pComparison = do
  left <- pArithExpr
  op <- choice
    [ Eq <$ symbol "==" 
    , Leq <$ try (symbol "<=")
    , Geq <$ try (symbol ">=")
    , Lt <$ (symbol "<" <* notFollowedBy (char '='))
    , Gt <$ (symbol ">" <* notFollowedBy (char '='))

    ]
  op left <$> pArithExpr

boolOpTable :: [[Operator Parser BoolExpr]]
boolOpTable = 
  [ [ prefix "not" Not ]
  , [ binary "and" And ]
  , [ binary "or" Or ] 
  ]

pBoolTerm :: Parser BoolExpr
pBoolTerm = choice
  [ pBoolLiteral
  , pComparison 
  , brackets pBoolExpr
  ]

pBoolExpr :: Parser BoolExpr
pBoolExpr = makeExprParser pBoolTerm boolOpTable

-- Command Parsing
pAssign :: Parser Command
pAssign = do
  Var varName <- pVariable <?> "variable assignment"
  _ <- symbol ":=" <?> "assignment operator"
  expr <- pArithExpr <?> "arithmetic expression"
  return (Assign varName expr)

pWhile :: Parser Command
pWhile = do
  _ <- symbol "while"
  cond <- pBoolExpr
  _ <- symbol "do"
  body <- pCommand
  _ <- symbol "od"
  return $ While cond body

pIf :: Parser Command
pIf = do
  _ <- symbol "if"
  cond <- pBoolExpr
  _ <- symbol "then"
  thenBody <- pCommand
  _ <- symbol "else"
  elseBody <- pCommand
  _ <- symbol "fi"
  return $ If cond thenBody elseBody

pSkip :: Parser Command
pSkip = Skip <$ symbol "skip"

pCommand :: Parser Command
pCommand = do
  cmds <- pSingleCommand `sepBy1` symbol ";"
  return $ if length cmds == 1 then head cmds else Seq cmds

pSingleCommand :: Parser Command
pSingleCommand = choice
  [ pWhile
  , pIf
  , pSkip
  , pAssign
  ]

pProgram :: Parser Command
pProgram = pCommand <* eof
