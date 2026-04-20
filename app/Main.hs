module Main (main) where

import Conductor (parseTest, runTests)
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runTests
        [path] -> do
            input <- TIO.readFile path
            parseTest input
        _ -> putStrLn "Usage: conductor [file]"
