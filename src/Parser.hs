{-# LANGUAGE OverloadedStrings #-}

module Parser where



import           Data.Text  (Text)
import           Data.Void  (Void)      
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parser Void Text

-- Define the fuhin comments

sc :: L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc 

symbol :: Text -> Parser Text
symbol = L.symbol sc
