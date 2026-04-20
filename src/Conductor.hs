module Conductor (
    parseTest,
    runTests,
    ScreenDimension (..),
    WindowTransform (..),
    WindowLayoutFunc,
    Config (..),
    mkConfig,
    parseConfig,
    eval,
    -- re-exports for tests
    Rule (..),
    Variable (..),
) where

import Control.Monad.Reader (Reader, ask)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Parser (Rule (..), Variable (..), parseConductor)
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

-- config for conductor

data Config = Config
    { startingRule :: Rule
    , rules :: Map Variable Rule
    }
    deriving (Show)

-- parse text directly into a config
parseConfig :: Text -> Either String Config
parseConfig input = case parse parseConductor "" input of
    Left err -> Left (errorBundlePretty err)
    Right rs -> case mkConfig rs of
        Nothing -> Left "empty program"
        Just cfg -> Right cfg

-- build config from parsed rules

mkConfig :: [Rule] -> Maybe Config
mkConfig [] = Nothing
mkConfig rs@(first : _) =
    Just $
        Config
            { startingRule = first
            , rules = Map.fromList [(name r, r) | r <- rs]
            }

-- evaluator 

-- runReader eval cfg :: WindowLayoutFunc


eval :: Reader Config WindowLayoutFunc
eval = do
    cfg <- ask
    return $ \(dim, windows) ->
        evalRule (startingRule cfg) (rules cfg) dim windows

evalRule :: Rule -> Map Variable Rule -> ScreenDimension -> [WindowId] -> [(WindowId, WindowTransform)]
evalRule = undefined
