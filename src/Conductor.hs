module Conductor (parseTest, runTests) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text, pack)
import Parser (parseConductor)
import Text.Megaparsec (errorBundlePretty, parse)
import Prelude hiding (readFile)

parseTest :: Text -> IO ()
parseTest input = case parse parseConductor "" input of
    Left err -> putStrLn $ "Error: " ++ errorBundlePretty err
    Right result -> BL.putStrLn (encode result)

runTests :: IO ()
runTests = do
    putStrLn "Testing parser..."
    parseTest $ pack "start = full [|] none\n"
    parseTest $ pack "end = none [-] full\n"
    parseTest $ pack "start = (full [|] full) [-] none\n"
    parseTest $ pack "start = full [|] ?stack\nstack = full (-) ?stack"
