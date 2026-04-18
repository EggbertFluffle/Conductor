{-# LANGUAGE OverloadedStrings #-}
module Parser where
import Data.Text (Text)
import Text.Megaparsec (
        Parsec, (<|>), 
        MonadParsec (notFollowedBy, takeWhile1P),
        some, optional, try, eof)
import Text.Megaparsec.Char (char, hspace, newline, string, alphaNumChar, digitChar)
import Data.Void (Void)
import Data.Char (isAlphaNum)
type Input = Text
type Parser a = Parsec Void Text a

-- <conductor> ::= {<rule>}
type Conductor = [Rule]

-- <rule> ::= <variable> "=" <expr>
data Rule = Rule Variable Expression
    deriving Show

-- <variable> ::= <letter> [{<letter> | <digit>}]
data Variable = Variable Text
    deriving Show

-- <expr> ::= <operand> <operator> [<opt>]<operand>
--          | <opt><operand> <operator> <operand>
--          | <operand>
data Expression
    = ExprBinary (Maybe Opt) Operand Operator (Maybe Opt) Operand
    | ExprUnary Operand
    deriving Show

-- <operand> ::= "(" <expr> ")" | <literal> | <variable>
data Operand
    = OperandParen Expression
    | OperandLit Litteral
    | OperandVar Variable
    deriving Show

-- <literal> ::= "full" | "none"
data Litteral = Full | None
    deriving Show

-- <operator> ::= <split> | <divide> | <layer>
data Operator
    = Split PartitionDirection (Maybe Param)
    | Divide PartitionDirection
    | Layer LayerSpec
    deriving Show

-- <part_dir> ::= "-" | "|"
data PartitionDirection = Horizontal | Vertical
    deriving Show

-- <layer_dir> ::= "<" | "^" | ">" | "v"
data LayerDir = LayerLeft | LayerUp | LayerRight | LayerDown
    deriving Show

-- <layer> ::= "{" <layer_dir> "}" | "{" <param> "," <param> "}"
data LayerSpec
    = LayerDirection LayerDir
    | LayerParams Param Param
    deriving Show

-- <param> ::= "param" | <float>
data Param = ParamKeyword | ParamFloat Float
    deriving Show

-- <opt> ::= "?"
data Opt = Opt
    deriving Show

parseConductor :: Parser Conductor
parseConductor = some parseRule <* eof

parseRule :: Parser Rule
parseRule = do
    variable <- parseVariable
    hspace
    _ <- char '='
    hspace
    expression <- parseExpression
    _ <- newline <|> (eof >> return '\n')
    return $ Rule variable expression

-- hspace :: Parser ()
-- hspace = do
--     _ <- optional (takeWhile1P (Just "space") (\c -> c == ' ' || c == '\t'))
--     return ()

parseVariable :: Parser Variable
parseVariable = try $ do
    t <- takeWhile1P (Just "variable") (\c -> isAlphaNum c || c == '_')
    notFollowedBy alphaNumChar
    case t of
        "full"  -> fail "expected variable, got keyword 'full'"
        "none"  -> fail "expected variable, got keyword 'none'"
        "param" -> fail "expected variable, got keyword 'param'"
        _       -> return $ Variable t

parseOperand :: Parser Operand
parseOperand =
      (OperandParen <$> (char '(' *> parseExpression <* char ')'))
  <|> (OperandLit <$> parseLitteral)
  <|> (OperandVar <$> parseVariable)

parseExpression :: Parser Expression
parseExpression = do
    optLeft  <- optional parseOpt
    operand1 <- parseOperand
    case optLeft of
        Just _ -> do
            hspace
            operator <- parseOperator
            hspace
            optRight <- optional parseOpt
            operand2 <- parseOperand
            return $ ExprBinary optLeft operand1 operator optRight operand2
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
parseOpt = do
    _ <- char '?'
    return Opt

parseLitteral :: Parser Litteral
parseLitteral =
      Full <$ string "full" <* notFollowedBy alphaNumChar
  <|> None <$ string "none" <* notFollowedBy alphaNumChar

parseOperator :: Parser Operator
parseOperator =
      parseSplit
  <|> parseDivide
  <|> parseLayer

parseSplit :: Parser Operator
parseSplit = do
    _ <- char '['
    dir   <- parsePartDir
    param <- optional (char ',' *> hspace *> parseParam)
    _ <- char ']'
    return $ Split dir param

parseDivide :: Parser Operator
parseDivide = try $ do
    _ <- char '('
    dir <- parsePartDir
    _ <- char ')'
    return $ Divide dir

parseLayer :: Parser Operator
parseLayer = do
    _ <- char '{'
    spec <- try (LayerDirection <$> parseLayerDir)
        <|> (LayerParams <$> parseParam <*> (char ',' *> hspace *> parseParam))
    _ <- char '}'
    return $ Layer spec

parsePartDir :: Parser PartitionDirection
parsePartDir =
      Horizontal <$ char '-'
  <|> Vertical   <$ char '|'

parseLayerDir :: Parser LayerDir
parseLayerDir =
      LayerLeft  <$ char '<'
  <|> LayerUp    <$ char '^'
  <|> LayerRight <$ char '>'
  <|> LayerDown  <$ char 'v'

parseParam :: Parser Param
parseParam =
      ParamKeyword <$ string "param" <* notFollowedBy alphaNumChar
  <|> ParamFloat   <$> parseFloat

parseFloat :: Parser Float
parseFloat = do
    whole <- some digitChar
    frac  <- optional (char '.' *> some digitChar)
    return $ read $ case frac of
        Just f  -> whole ++ "." ++ f
        Nothing -> whole
