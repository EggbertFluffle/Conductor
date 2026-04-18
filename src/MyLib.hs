module MyLib (main, parseTest) where

import Parser (parseConductor)
import Data.Text (Text, pack)
import Data.Text.IO (readFile)
import Text.Megaparsec (parse, errorBundlePretty)
import System.Environment (getArgs)
import Prelude hiding (readFile)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL

parseTest :: Text -> IO ()
parseTest input = case parse parseConductor "" input of
    Left err     -> putStrLn $ "Error: " ++ errorBundlePretty err
    Right result -> BL.putStrLn (encode result)

runTests :: IO ()
runTests = do
    putStrLn "Testing parser..."
    parseTest $ pack "start = full [|] none\n"
    parseTest $ pack "end = none [-] full\n"
    parseTest $ pack "start = (full [|] full) [-] none\n"
    parseTest $ pack "start = full [|] ?stack\nstack = full (-) ?stack"

main :: IO ()
main = do
    args <- getArgs
    case args of
        []         -> runTests
        [path]     -> do
            input <- readFile path
            parseTest input
        _          -> putStrLn "Usage: conductor [file]"
