{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Conductor.Parser (
    parseConductor,
    Conductor,
    Rule (..),
    Variable (..),
    Expression (..),
    Operand (..),
    Litteral (..),
    Operator (..),
    PartitionDirection (..),
    LayerDir (..),
    LayerSpec (..),
    Param (..),
    Opt (Opt),
) where

import Data.Aeson (FromJSON, ToJSON)

import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (
    MonadParsec (notFollowedBy, takeWhile1P),
    Parsec,
    eof,
    optional,
    some,
    try,
    (<|>),
  )
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, hspace, newline, string)

import GHC.Generics (Generic)

type Input = Text
type Parser a = Parsec Void Input a

-- <conductor> ::= {<rule>}
type Conductor = [Rule]

-- <rule> ::= <variable> "=" <expr>
data Rule = Rule {name :: Variable, r_expression :: Expression}
    deriving (Show, Generic)

instance FromJSON Rule
instance ToJSON Rule

-- <variable> ::= <letter> [{<letter> | <digit>}]
newtype Variable = Variable Text
    deriving (Show, Eq, Ord, Generic)

instance FromJSON Variable
instance ToJSON Variable

-- <expr> ::= <operand> <operator> [<opt>]<operand>
--          | <opt><operand> <operator> <operand>
--          | <operand>
data Expression
    = ExprBinary (Maybe Opt) Operand Operator (Maybe Opt) Operand
    | ExprUnary Operand
    deriving (Show, Generic)

instance FromJSON Expression
instance ToJSON Expression

-- <operand> ::= "(" <expr> ")" | <literal> | <variable>
data Operand
    = OperandParen {o_expression :: Expression}
    | OperandLit {litteral :: Litteral}
    | OperandVar {variable :: Variable}
    deriving (Show, Generic)

instance FromJSON Operand
instance ToJSON Operand

-- <literal> ::= "full" | "none"
data Litteral = Full | None
    deriving (Show, Generic)

instance FromJSON Litteral
instance ToJSON Litteral

-- <operator> ::= <split> | <divide> | <layer>
data Operator
    = Split {sDirection :: PartitionDirection, sParams :: Maybe Param}
    | Divide {dDirection :: PartitionDirection}
    | Layer LayerSpec
    deriving (Show, Generic)

instance FromJSON Operator
instance ToJSON Operator

-- <part_dir> ::= "-" | "|"
data PartitionDirection = Horizontal | Vertical
    deriving (Show, Generic)

instance FromJSON PartitionDirection
instance ToJSON PartitionDirection

-- <layer_dir> ::= "<" | "^" | ">" | "v"
data LayerDir = LayerLeft | LayerUp | LayerRight | LayerDown
    deriving (Show, Generic)

instance FromJSON LayerDir
instance ToJSON LayerDir

-- <layer> ::= "{" <layer_dir> "}" | "{" <param> "," <param> "}"
data LayerSpec
    = LayerDirection {layerDirection :: LayerDir}
    | LayerParams {layX :: Param, layY :: Param}
    deriving (Show, Generic)

instance FromJSON LayerSpec
instance ToJSON LayerSpec

-- <param> ::= "param" | <float>
data Param = ParamKeyword | ParamFloat Float
    deriving (Show, Generic)

instance FromJSON Param
instance ToJSON Param

-- <opt> ::= "?"
data Opt = Opt
    deriving (Show, Generic)

instance FromJSON Opt
instance ToJSON Opt

parseConductor :: Parser Conductor
parseConductor = some parseRule <* eof

parseRule :: Parser Rule
parseRule = do
    var <- parseVariable
    hspace
    _ <- char '='
    hspace
    expr <- parseExpression
    _ <- (semicolonEnd <|> newlineEnd)
    return $ Rule var expr
  where
    semicolonEnd = do
        _ <- char ';'
        hspace
        newline <|> (eof >> return '\n')
    newlineEnd = newline <|> (eof >> return '\n')

parseVariable :: Parser Variable
parseVariable = try $ do
    t <- takeWhile1P (Just "variable") (\c -> isAlphaNum c || c == '_')
    notFollowedBy alphaNumChar
    case t of
        "full" -> fail "expected variable, got keyword 'full'"
        "none" -> fail "expected variable, got keyword 'none'"
        "param" -> fail "expected variable, got keyword 'param'"
        _ -> return $ Variable t

parseOperand :: Parser Operand
parseOperand =
    (OperandParen <$> (char '(' *> parseExpression <* char ')'))
        <|> (OperandLit <$> parseLitteral)
        <|> (OperandVar <$> parseVariable)

parseExpression :: Parser Expression
parseExpression = do
    optLeft <- optional parseOpt
    operand1 <- parseOperand
    case optLeft of
        Just _ ->
            ExprBinary optLeft operand1
                <$> (hspace *> parseOperator)
                <*> (hspace *> optional parseOpt)
                <*> parseOperand
        Nothing -> do
            rest <- optional $ try $ do
                hspace
                operator <- parseOperator
                hspace
                optRight <- optional parseOpt
                operand2 <- parseOperand
                return (operator, optRight, operand2)
            return $ case rest of
                Just (op, optRight, operand2) ->
                    ExprBinary Nothing operand1 op optRight operand2
                Nothing ->
                    ExprUnary operand1

parseOpt :: Parser Opt
parseOpt = Opt <$ char '?'

parseLitteral :: Parser Litteral
parseLitteral =
    Full
        <$ string "full"
        <* notFollowedBy alphaNumChar
            <|> None
        <$ string "none"
        <* notFollowedBy alphaNumChar

parseOperator :: Parser Operator
parseOperator = parseSplit <|> parseDivide <|> parseLayer

parseSplit :: Parser Operator
parseSplit = Split <$> (char '[' *> parsePartDir) <*> (optional (char ',' *> hspace *> parseParam) <* char ']')

parseDivide :: Parser Operator
parseDivide = try $ Divide <$> (char '(' *> parsePartDir <* char ')')

parseLayer :: Parser Operator
parseLayer = do
    _ <- char '{'
    spec <-
        try (LayerDirection <$> parseLayerDir)
            <|> (LayerParams <$> parseParam <*> (char ',' *> hspace *> parseParam))
    _ <- char '}'
    return $ Layer spec

parsePartDir :: Parser PartitionDirection
parsePartDir =
    Horizontal
        <$ char '-'
            <|> Vertical
        <$ char '|'

parseLayerDir :: Parser LayerDir
parseLayerDir =
    LayerLeft
        <$ char '<'
            <|> LayerUp
        <$ char '^'
            <|> LayerRight
        <$ char '>'
            <|> LayerDown
        <$ char 'v'

parseParam :: Parser Param
parseParam =
    ParamKeyword
        <$ string "param"
        <* notFollowedBy alphaNumChar
            <|> ParamFloat
        <$> parseFloat

parseFloat :: Parser Float
parseFloat = do
    whole <- some digitChar
    frac <- optional (char '.' *> some digitChar)
    return $ read $ case frac of
        Just f -> whole ++ "." ++ f
        Nothing -> whole
