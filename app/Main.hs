{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Conductor
import Conductor.Parser
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), eitherDecode, encode, (.:), (.=), object, withObject)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import System.Environment (getArgs)
import Text.Megaparsec (parse)

data InputJSON = InputJSON
    { startingVariable :: Text
    , snippet :: Text
    , maxDepth :: Int
    , screenSize :: ScreenDimension
    , windowIds :: [WindowId]
    } deriving (Show, Generic)

data ScreenSizeJSON = ScreenSizeJSON
    { ssWidth :: Int
    , ssHeight :: Int
    } deriving (Show, Generic)

instance FromJSON ScreenSizeJSON where
    parseJSON = withObject "ScreenSize" $ \o -> do
        w <- o .: "width"
        h <- o .: "height"
        return ScreenSizeJSON { ssWidth = w, ssHeight = h }

instance FromJSON InputJSON where
    parseJSON = withObject "InputJSON" $ \o -> do
        sv <- o .: "starting_variable"
        sn <- o .: "snippet"
        md <- o .: "max_depth"
        ss <- o .: "screen_size"
        wid <- o .: "window_ids"
        return InputJSON
            { startingVariable = sv
            , snippet = sn
            , maxDepth = md
            , screenSize = ScreenDimension (ssWidth ss) (ssHeight ss)
            , windowIds = wid
            }

data PlacementJSON = PlacementJSON
    { pId :: WindowId
    , pTransform :: RectJSON
    } deriving (Show, Generic)

data RectJSON = RectJSON
    { rx :: Int
    , ry :: Int
    , rw :: Int
    , rh :: Int
    } deriving (Show, Generic)

instance ToJSON PlacementJSON where
    toJSON p = object
        [ "id" .= pId p
        , "transform" .= pTransform p
        ]

instance ToJSON RectJSON where
    toJSON r = object
        [ "x" .= rx r
        , "y" .= ry r
        , "width" .= rw r
        , "height" .= rh r
        ]

data OutputJSON = OutputJSON
    { placements :: [PlacementJSON]
    , ignored :: [WindowId]
    } deriving (Show, Generic)

instance ToJSON OutputJSON

main :: IO ()
main = do
    args <- getArgs
    inputText <- case args of
        [] -> TIO.getContents
        [path] -> TIO.readFile path
        _ -> do
            putStrLn "Usage: conductor [file]"
            error "Invalid arguments"

    let jsonResult = eitherDecode (BL.fromStrict (T.encodeUtf8 inputText)) :: Either String InputJSON
    case jsonResult of
        Left err -> do
            putStrLn $ "JSON parse error: " ++ err
            error "Failed to parse input JSON"
        Right inp -> do
            let parseResult = parse parseConductor "" (snippet inp)
            case parseResult of
                Left e -> do
                    putStrLn $ "Parser error: " ++ show e
                    error "Failed to parse snippet"
                Right rules ->
                    let ruleMap = foldr (\r m -> M.insert (name r) (r_expression r) m) M.empty rules
                        cfg = Config
                            { cStartVariable = Variable (startingVariable inp)
                            , cRuleMap = ruleMap
                            , cScreenDimension = screenSize inp
                            , cMaxDepth = maxDepth inp
                            }
                        (placed, ignoredWins) = runEvalRules cfg (windowIds inp) []
                        toPlacementJSON (wid, rect) = PlacementJSON
                            { pId = wid
                            , pTransform = RectJSON
                                { rx = rX rect
                                , ry = rY rect
                                , rw = rW rect
                                , rh = rH rect
                                }
                            }
                        placementsJson = map toPlacementJSON placed
                        output = OutputJSON
                            { placements = placementsJson
                            , ignored = ignoredWins
                            }
                     in BL.putStrLn (encode output)