module Main (main, parseTest) where

import Parser (parseRule)
import Data.Text (Text, pack)
import Text.Megaparsec (parse, errorBundlePretty)

parseTest :: Text -> IO ()
parseTest input = case parse parseRule "" input of
    Left err -> putStrLn $ "Error: " ++ errorBundlePretty err
    Right result -> print result

main :: IO ()
main = do
    putStrLn "Testing parser..."
    parseTest $ pack "start = full [|] none\n"
    parseTest $ pack "end = none [-] full\n"
    parseTest $ pack "start = (full [|] full) - none\n"
