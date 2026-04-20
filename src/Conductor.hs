module Conductor (
    parseTest,
    runTests,
    PointerCoordinate (..),
    ScreenDimension (..),
    WindowSpec (..),
    WindowMapping (..),
    WindowLayoutFunc,
) where

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

-- screen size pixel
data ScreenDimension = ScreenDimension
    { sdWidth :: Int
    , sdHeight :: Int
    }
    deriving (Show, Eq)

type WindowId = Int

-- window mapping
data WindowTransform = WindowTransform
    { wtX :: Int
    , wtY :: Int
    , wtWidth :: Int
    , wtHeight :: Int
    }
    deriving (Show, Eq)

-- XMonad layout call type shi call laypit func
type WindowLayoutFunc =
    (ScreenDimension, [WindowId]) -> [(WindowId, WindowTransform)]
