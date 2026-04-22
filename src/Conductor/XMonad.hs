module Conductor.XMonad (
    ConductorLayout (..),
    conductorLayout,
) where

import Conductor (Config (..), Rect (..), ScreenDimension (..), mkConfig, runConductor)
import Conductor.Parser (Rule (..), Variable (..))
import qualified Data.List as L
import Graphics.X11.Types (Rectangle (..))
import XMonad (LayoutClass (..), Window, X)
import qualified XMonad.StackSet as W

-- XMonad layout wrapper that holds the parsed conductor config
-- The screen dimension in Config is updated each layout call from
-- the Rectangle XMonad provides, so it stays in sync with the WM
data ConductorLayout a = ConductorLayout
    { clRules :: [Rule]
    -- ^ Parsed rules from the .conductor file
    , clStartVar :: Variable
    -- ^ Entry point variable (first rule by default)
    , clMaxDepth :: Int
    -- ^ Recursion cap for variable references
    }
    deriving (Show, Read)

-- construct a ConductorLayout from parsed rules.
-- use the first rule as the entry point.
conductorLayout :: [Rule] -> Int -> ConductorLayout a
conductorLayout rs maxDepth =
    ConductorLayout
        { clRules = rs
        , clStartVar = case rs of
            [] -> Variable "start"
            (r : _) -> name r
        , clMaxDepth = maxDepth
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

-- pure layout computation. Rebuilds the WindowLayoutFunc on every call
-- so the screen rect stays current.
runLayout' :: ConductorLayout Window -> Rectangle -> [Window] -> [(Window, Rectangle)]
runLayout' cl xRect wins =
    let dim = rectToScreenDim xRect
        cfg = mkConfig (clRules cl) (clStartVar cl) dim (clMaxDepth cl)
    in case runConductor cfg of
        Left _ -> fallbackLayout xRect wins
        Right fn ->
            let wids = map (fromIntegral . fromEnum) wins
                (placements, _leftover) = fn wids []
            in [(toEnum wid, rectFromRect r) | (wid, r) <- placements]




-- Type conversions

-- | XMonad rectangle to ScreenDimension
rectToScreenDim :: Rectangle -> ScreenDimension
rectToScreenDim r =
    ScreenDimension
        { sdWidth = fromIntegral (rect_width r)
        , sdHeight = fromIntegral (rect_height r)
        }

-- our rectangle to XMonad Rectangle
-- XMonad Rectangle origin is absolute on screen; our Rect origin is relative
-- to the workspace rect, so we offset by the workspace x/y.
rectFromRect :: Rect -> Rectangle
rectFromRect (Rect x y w h) =
    Rectangle
        { rect_x = fromIntegral x
        , rect_y = fromIntegral y
        , rect_width = fromIntegral w
        , rect_height = fromIntegral h
        }


-- incase omething gets messed up hecka bad

-- stack all windows vertically if conductor eval fails.
fallbackLayout :: Rectangle -> [Window] -> [(Window, Rectangle)]
fallbackLayout _ [] = []
fallbackLayout rect wins =
    let n = length wins
        slotH = rect_height rect `div` fromIntegral n
    in zipWith
        ( \i w ->
            ( w
            , rect
                { rect_y = rect_y rect + fromIntegral i * fromIntegral slotH
                , rect_height = slotH
                }
            )
        )
        [0 ..]
        wins
