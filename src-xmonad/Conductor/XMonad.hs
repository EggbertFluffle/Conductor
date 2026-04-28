{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Conductor.XMonad (
    ConductorLayout (..),
    conductorLayout,
    conductorLayoutFromFile,
    conductorLayoutFromJSON,
    XMonadConfig (..),
) where

import Conductor (Config (..), Rect (..), ScreenDimension (..), runEvalRules)
import Conductor.Parser (Rule (..), Variable (..))
import Data.Aeson (FromJSON, eitherDecode, parseJSON, withObject, (.!=), (.:), (.:?))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import XMonad (LayoutClass (..), Rectangle (..), Window)
import qualified XMonad.StackSet as W

-- JSON config loaded from file. screen_size and window_ids are supplied
data XMonadConfig = XMonadConfig
    { xcStartVariable :: Variable
    -- ^ Entry-point rule name (e.g. "start")
    , xcRules :: [Rule]
    -- ^ All layout rules
    , xcMaxDepth :: Int
    -- ^ Recursion cap (default 20)
    }

instance FromJSON XMonadConfig where
    parseJSON = withObject "XMonadConfig" $ \o -> do
        sv <- o .: "startVariable"
        rs <- o .: "rules"
        md <- o .:? "maxDepth" .!= 20
        return
            XMonadConfig
                { xcStartVariable = sv
                , xcRules = rs
                , xcMaxDepth = md
                }

data ConductorLayout a = ConductorLayout
    { clRules :: [Rule]
    -- ^ Parsed rules from the conductor config
    , clStartVar :: Variable
    -- ^ Entry-point variable (first rule by default)
    , clMaxDepth :: Int
    -- ^ Recursion cap for variable references
    }
    deriving (Show, Read)

{- | Build a layout directly from a list of rules.
Uses the first rule as the entry point.
-}
conductorLayout :: [Rule] -> Int -> ConductorLayout a
conductorLayout rs maxDepth =
    ConductorLayout
        { clRules = rs
        , clStartVar = case rs of
            [] -> Variable "start"
            (r : _) -> name r
        , clMaxDepth = maxDepth
        }

{- | Load a layout from a JSON file.
JSON format:
  { "startVariable": "start"
  , "rules": [...]
  , "maxDepth": 20        -- optional, default 20
  }
-}
conductorLayoutFromFile :: FilePath -> IO (ConductorLayout a)
conductorLayoutFromFile path = do
    bs <- BL.readFile path
    case conductorLayoutFromJSON bs of
        Left err -> ioError (userError $ "Conductor: bad JSON config: " ++ err)
        Right cl -> return cl

-- | layout from a lazy ByteString (the JSON shown above).
conductorLayoutFromJSON :: BL.ByteString -> Either String (ConductorLayout a)
conductorLayoutFromJSON bs = case eitherDecode bs of
    Left err -> Left err
    Right xc ->
        Right
            ConductorLayout
                { clRules = xcRules xc
                , clStartVar = xcStartVariable xc
                , clMaxDepth = xcMaxDepth xc
                }

instance LayoutClass ConductorLayout Window where
    -- XMonad calls doLayout each time it needs to arrange windows.
    -- rect: the screen area available (changes with bars, multi-monitor, etc.)
    -- stack: the focused window stack for this workspace
    doLayout cl rect stack = do
        let wins = W.integrate stack
            placements = runLayout' cl rect wins
        return (placements, Nothing)

    description _ = "Conductor"

-- Pure layout computation rebuilds cfg on every call so the screen
-- rect stays current.
runLayout' :: ConductorLayout Window -> Rectangle -> [Window] -> [(Window, Rectangle)]
runLayout' cl xRect wins =
    let dim = rectToScreenDim xRect
        ruleMap = foldr (\r m -> M.insert (name r) (r_expression r) m) M.empty (clRules cl)
        cfg     = Config
            { cStartVariable  = clStartVar cl
            , cRuleMap        = ruleMap
            , cScreenDimension = dim
            , cMaxDepth       = clMaxDepth cl
            }
        wids            = map fromIntegral wins
        (placed, _)     = runEvalRules cfg wids []
        ox              = fromIntegral (rect_x xRect)
        oy              = fromIntegral (rect_y xRect)
        converted       = [(fromIntegral wid, rectFromRect ox oy r) | (wid, r) <- placed]
    -- fall back to vertical stack if the evaluator produced no placements
    in if null converted then fallbackLayout xRect wins else converted

-- | XMonad Rectangle → ScreenDimension
rectToScreenDim :: Rectangle -> ScreenDimension
rectToScreenDim r =
    ScreenDimension
        { sdWidth = fromIntegral (rect_width r)
        , sdHeight = fromIntegral (rect_height r)
        }

-- |XMonad Rectangle
-- conductor computes positions relative to (0,0); add the workspace
-- origin so the result is in absolute screen coordinates.
rectFromRect :: Int -> Int -> Rect -> Rectangle
rectFromRect ox oy (Rect x y w h) =
    Rectangle
        { rect_x      = fromIntegral (x + ox)
        , rect_y      = fromIntegral (y + oy)
        , rect_width  = fromIntegral w
        , rect_height = fromIntegral h
        }

-- | vertical stack fallback used when conductor eval fails.
fallbackLayout :: Rectangle -> [Window] -> [(Window, Rectangle)]
fallbackLayout _ [] = []
fallbackLayout rect wins =
    let n = length wins
        slotH = rect_height rect `div` fromIntegral n
     in zipWith
            ( \i w ->
                ( w
                , rect
                    { rect_y = rect_y rect + fromIntegral (i :: Int) * fromIntegral slotH
                    , rect_height = slotH
                    }
                )
            )
            [0 ..]
            wins
