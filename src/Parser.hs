{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

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

import Data.Aeson (ToJSON, object, toJSON, (.=))
import GHC.Generics (Generic)

type Input = Text
type Parser a = Parsec Void Text a

-- <conductor> ::= {<rule>}
type Conductor = [Rule]

-- <rule> ::= <variable> "=" <expr>
data Rule = Rule {name :: Variable, r_expression :: Expression}
    deriving (Show, Generic)

-- <variable> ::= <letter> [{<letter> | <digit>}]
newtype Variable = Variable Text
    deriving (Show, Eq, Ord, Generic)

-- <expr> ::= <operand> <operator> [<opt>]<operand>
--          | <opt><operand> <operator> <operand>
--          | <operand>
data Expression
    = ExprBinary (Maybe Opt) Operand Operator (Maybe Opt) Operand
    | ExprUnary Operand
    deriving (Show, Generic)

-- <operand> ::= "(" <expr> ")" | <literal> | <variable>
data Operand
    = OperandParen {o_expression :: Expression}
    | OperandLit {litteral :: Litteral}
    | OperandVar {variable :: Variable}
    deriving (Show, Generic)

-- <literal> ::= "full" | "none"
data Litteral = Full | None
    deriving (Show, Generic)

-- <operator> ::= <split> | <divide> | <layer>
data Operator
    = Split {direction :: PartitionDirection, params :: Maybe Param}
    | Divide {direction :: PartitionDirection}
    | Layer LayerSpec
    deriving (Show, Generic)

-- <part_dir> ::= "-" | "|"
data PartitionDirection = Horizontal | Vertical
    deriving (Show, Generic)

-- <layer_dir> ::= "<" | "^" | ">" | "v"
data LayerDir = LayerLeft | LayerUp | LayerRight | LayerDown
    deriving (Show, Generic)

-- <layer> ::= "{" <layer_dir> "}" | "{" <param> "," <param> "}"
data LayerSpec
    = LayerDirection {layer_direction :: LayerDir}
    | LayerParams {x :: Param, y :: Param}
    deriving (Show, Generic)

-- <param> ::= "param" | <float>
data Param = ParamKeyword | ParamFloat Float
    deriving (Show, Generic)

-- <opt> ::= "?"
data Opt = Opt
    deriving (Show, Generic)

instance ToJSON Rule
instance ToJSON Variable
instance ToJSON PartitionDirection
instance ToJSON LayerDir
instance ToJSON LayerSpec

instance ToJSON Param where
    toJSON ParamKeyword = object ["param_type" .= ("runtime" :: Text)]
    toJSON (ParamFloat f) = object ["param_type" .= ("float" :: Text), "value" .= f]

instance ToJSON Opt where
    toJSON Opt = toJSON ()

instance ToJSON Expression where
    toJSON (ExprBinary optL op oper optR right) =
        object
            [ "expression_type" .= ("binary" :: Text)
            , "optional" .= case (optL, optR) of
                (Just _, Nothing) -> ("left" :: Text)
                _ -> "right"
            , "left_operand" .= op
            , "operator" .= oper
            , "right_operand" .= right
            ]
    toJSON (ExprUnary op) =
        object
            [ "expression_type" .= ("unary" :: Text)
            , "operand" .= op
            ]

instance ToJSON Operand where
    toJSON (OperandParen e) =
        object
            [ "operand_type" .= ("paren" :: Text)
            , "expression" .= e
            ]
    toJSON (OperandLit l) =
        object
            [ "operand_type" .= ("literal" :: Text)
            , "value"
                .= ( case l of
                        Full -> ("full" :: Text)
                        None -> ("none" :: Text)
                   )
            ]
    toJSON (OperandVar v) =
        object
            [ "operand_type" .= ("variable" :: Text)
            , "name" .= v
            ]

instance ToJSON Operator where
    toJSON (Split dir mParam) =
        object
            [ "operator_type" .= ("split" :: Text)
            , "direction" .= dir
            , "params" .= case mParam of
                Just p -> [toJSON p]
                Nothing -> [toJSON $ ParamFloat 0.5]
            ]
    toJSON (Divide dir) =
        object
            [ "operator_type" .= ("divide" :: Text)
            , "direction" .= dir
            ]
    toJSON (Layer spec) =
        object
            [ "operator_type" .= ("layer" :: Text)
            , "spec" .= case spec of
                LayerDirection LayerLeft -> object ["x" .= (-1 :: Float), "y" .= (0 :: Float)]
                LayerDirection LayerRight -> object ["x" .= (1 :: Float), "y" .= (0 :: Float)]
                LayerDirection LayerUp -> object ["x" .= (0 :: Float), "y" .= (-1 :: Float)]
                LayerDirection LayerDown -> object ["x" .= (0 :: Float), "y" .= (1 :: Float)]
                LayerParams x' y' -> object ["x" .= x', "y" .= y']
            ]

parseConductor :: Parser Conductor
parseConductor = some parseRule <* eof

parseRule :: Parser Rule
parseRule = do
    var <- parseVariable
    hspace
    _ <- char '='
    hspace
    expr <- parseExpression
    _ <- newline <|> (eof >> return '\n')
    return $ Rule var expr

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
