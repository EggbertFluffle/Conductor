{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Text (Text)
import Text.Megaparsec (
        Parsec,
        oneOf,
        (<|>),
        MonadParsec (notFollowedBy, takeWhile1P), some)
import Text.Megaparsec.Char (char, space1, newline, string, alphaNumChar)
import Data.Void (Void)
import Data.Char (isAlpha)

type Input = Text

type Parser a = Parsec Void Text a

type Conductor = [Rule]

data Rule = Rule Variable Expression
    deriving Show

data Variable = Variable Text
    deriving Show

data Expression = Expression Litteral Operator Litteral
    deriving Show

data Operand = OperandParen Expression | OperandLit Litteral | OperandVar Variable
    deriving Show

data Litteral = Full | None
    deriving Show

data Operator = Split PartitionDirection
    deriving Show

data PartitionDirection = Horizontal | Vertical
    deriving Show

parseConductor :: Parser Conductor
parseConductor = some parseRule

parseRule :: Parser Rule
parseRule = do
    variable <- parseVariable
    space1
    _ <- char '='
    space1
    expression <- parseExpression
    _ <- newline
    return $ Rule variable expression

parseVariable :: Parser Variable
parseVariable = do
    t <- takeWhile1P (Just "variable") isAlpha 
    return $ Variable t

parseOperand :: Parser Operand
parseOperand = do
    operand <- OperandParen <$> (char '(' *> parseExpression <* char ')')
           <|> OperandLit <$> parseLitteral
           <|> OperandVar <$> parseVariable
    return operand

parseExpression :: Parser Expression
parseExpression = do
    litteral1 <- parseLitteral
    space1
    operator <- parseOperator
    space1
    litteral2 <- parseLitteral
    return $ Expression litteral1 operator litteral2

keyword :: Text -> Parser Text
keyword kw = string kw <* notFollowedBy alphaNumChar 

parseLitteral :: Parser Litteral
parseLitteral =
      Full <$ (string "full" <* notFollowedBy alphaNumChar)
  <|> None <$ (string "none" <* notFollowedBy alphaNumChar)
  
parseOperator :: Parser Operator
parseOperator = do
    p <- oneOf ['|', '-']
    return $ Split (if p == '|' then Vertical else Horizontal)

parseSplit :: Parser Operator
parseSplit = do
    p <- char '[' *> oneOf ['|', '-'] <* char ']'
    return $ Split (if p == '|' then Vertical else Horizontal)
