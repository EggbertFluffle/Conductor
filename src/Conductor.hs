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

-- winow types
-- poniter coordinate
data PointerCoordinate = PointerCoordinate
    { pcX :: Double
    , pcY :: Double
    }
    deriving (Show, Eq)

-- screen size pixel
data ScreenDimension = ScreenDimension
    { sdWidth :: Int
    , sdHeight :: Int
    }
    deriving (Show, Eq)

-- window handle
newtype WindowSpec = WindowSpec {windowId :: Int}
    deriving (Show, Eq, Ord)

-- window mapping
data WindowMapping = WindowMapping
    { wmWindow :: WindowSpec
    , wmX :: Int
    , wmY :: Int
    , wmWidth :: Int
    , wmHeight :: Int
    }
    deriving (Show, Eq)

-- XMonad layout call type shi call laypit func
type WindowLayoutFunc =
    (PointerCoordinate, ScreenDimension, [WindowSpec]) -> [WindowMapping]
