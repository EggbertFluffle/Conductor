{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Writer (runWriter)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec (errorBundlePretty, parse)

import Conductor (
    Config (..),
    Rect (..),
    ScreenDimension (..),
    WindowLayoutFunc,
    evalRules,
 )
import Conductor.Parser (Rule (..), Variable (..), parseConductor)

-- Helpers
mustParse :: Text -> [Rule]
mustParse src = case parse parseConductor "" src of
    Left err -> error $ "parse setup failed:\n" ++ errorBundlePretty err
    Right rs -> rs

mkConfig :: Text -> Variable -> ScreenDimension -> Int -> Config
mkConfig src start sd maxDepth =
    let rules = mustParse src
        rmap = M.fromList [(name r, r_expression r) | r <- rules]
     in Config
            { cStartVariable = start
            , cRuleMap = rmap
            , cScreenDimension = sd
            , cMaxDepth = maxDepth
            }

compileConfig :: Config -> (Maybe WindowLayoutFunc, [String])
compileConfig cfg = runWriter (runReaderT evalRules cfg)

compile :: Config -> WindowLayoutFunc
compile cfg = case fst (compileConfig cfg) of
    Just f -> f
    Nothing -> error "compile: evalRules returned Nothing"

screen :: ScreenDimension
screen = ScreenDimension 800 600

v :: Text -> Variable
v = Variable

-- Test 1: start = full [|] none
-- Implicit-? on right, `none` demands 0 → right collapses → `full`
-- takes the entire screen (not the left half).
layoutTest1 :: TestTree
layoutTest1 = testCase "1: full [|] none" $ do
    let cfg = mkConfig "start = full [|] none\n" (v "start") screen 100
        layout = compile cfg

    let (ps0, left0) = layout [] []
    assertEqual "0 windows: no placements" [] ps0
    assertEqual "0 windows: no leftover" [] left0

    let (ps1, left1) = layout [0] []
    assertEqual
        "1 window: takes full screen (right collapses)"
        [(0, Rect 0 0 800 600)]
        ps1
    assertEqual "1 window: no leftover" [] left1

    let (ps2, left2) = layout [0, 1] []
    assertEqual
        "2 windows: first takes full screen"
        [(0, Rect 0 0 800 600)]
        ps2
    assertEqual "2 windows: second is leftover" [1] left2

-- Test 2: end = none [-] full (start = "end")
-- `full` has demand 1, so right-? does not collapse (unlike test 1).
-- Horizontal split: top=none (empty), bottom=full (one window).
layoutTest2 :: TestTree
layoutTest2 = testCase "2: none [-] full (start=end)" $ do
    let cfg = mkConfig "end = none [-] full\n" (v "end") screen 100
        layout = compile cfg

    let (ps0, _) = layout [] []
    assertEqual "0 windows: collapse to none, nothing placed" [] ps0

    let (ps1, left1) = layout [0] []
    assertEqual
        "1 window: lands in bottom half"
        [(0, Rect 0 300 800 300)]
        ps1
    assertEqual "1 window: no leftover" [] left1

    let (ps2, left2) = layout [0, 1] []
    assertEqual
        "2 windows: still only one bottom slot"
        [(0, Rect 0 300 800 300)]
        ps2
    assertEqual "2 windows: second is leftover" [1] left2

{- | Companion to test 2: start variable not in the rule map should
  make evalRules return Nothing and log a message.
-}
layoutTest2UnknownStart :: TestTree
layoutTest2UnknownStart = testCase "2a: unknown start variable" $ do
    let cfg = mkConfig "end = none [-] full\n" (v "start") screen 100
        (mLayout, logs) = compileConfig cfg
    assertBool "returns Nothing" (case mLayout of Nothing -> True; Just _ -> False)
    assertEqual "logs exactly one message" 1 (length logs)

-- Test 3: start = (full [|] full) [-] none
-- Outer right-? collapses none (same pattern as test 1). Inner
-- `full [|] full` does a normal vertical split for 2 windows.
layoutTest3 :: TestTree
layoutTest3 = testCase "3: (full [|] full) [-] none" $ do
    let cfg = mkConfig "start = (full [|] full) [-] none\n" (v "start") screen 100
        layout = compile cfg

    let (ps0, _) = layout [] []
    assertEqual "0 windows: nothing placed" [] ps0

    let (ps1, left1) = layout [0] []
    assertEqual
        "1 window: inner right collapses, left full gets everything"
        [(0, Rect 0 0 800 600)]
        ps1
    assertEqual "1 window: no leftover" [] left1

    let (ps2, left2) = layout [0, 1] []
    assertEqual
        "2 windows: inner vertical split"
        [(0, Rect 0 0 400 600), (1, Rect 400 0 400 600)]
        ps2
    assertEqual "2 windows: no leftover" [] left2

    let (ps3, left3) = layout [0, 1, 2] []
    assertEqual
        "3 windows: only 2 placed"
        [(0, Rect 0 0 400 600), (1, Rect 400 0 400 600)]
        ps3
    assertEqual "3 windows: third is leftover" [2] left3

-- Test 4: master/stack
--   start = full [|] ?stack
--   stack = full (-) ?stack
layoutTest4 :: TestTree
layoutTest4 = testCase "4: master/stack recursion" $ do
    let src =
            "start = full [|] ?stack\n\
            \stack = full (-) ?stack\n"
        cfg = mkConfig src (v "start") screen 100
        layout = compile cfg

    let (ps0, _) = layout [] []
    assertEqual "0 windows: nothing placed" [] ps0

    let (ps1, left1) = layout [0] []
    assertEqual
        "1 window: ?stack collapses, full takes screen"
        [(0, Rect 0 0 800 600)]
        ps1
    assertEqual "1 window: no leftover" [] left1

    let (ps2, left2) = layout [0, 1] []
    assertEqual
        "2 windows: master on left half, one on right half"
        [(0, Rect 0 0 400 600), (1, Rect 400 0 400 600)]
        ps2
    assertEqual "2 windows: no leftover" [] left2

    let (ps3, left3) = layout [0, 1, 2] []
    assertEqual
        "3 windows: master + two stacked horizontally"
        [ (0, Rect 0 0 400 600)
        , (1, Rect 400 0 400 300)
        , (2, Rect 400 300 400 300)
        ]
        ps3
    assertEqual "3 windows: no leftover" [] left3

    let (ps4, left4) = layout [0, 1, 2, 3] []
    assertEqual
        "4 windows: master + three evenly stacked"
        [ (0, Rect 0 0 400 600)
        , (1, Rect 400 0 400 200)
        , (2, Rect 400 200 400 200)
        , (3, Rect 400 400 400 200)
        ]
        ps4
    assertEqual "4 windows: no leftover" [] left4

-- Split with explicit ratio 0.3: left gets 30% width, right gets 70%.
splitRatioTestLiteral :: TestTree
splitRatioTestLiteral = testCase "full [|, 0.3] full" $ do
    let cfg = mkConfig "start = full [|, 0.3] full\n" (v "start") screen 100
        layout = compile cfg
        (ps, left) = layout [0, 1] []
    assertEqual
        "2 windows: 30/70 vertical split"
        [(0, Rect 0 0 240 600), (1, Rect 240 0 560 600)]
        ps
    assertEqual "no leftover" [] left

-- Split with `param` keyword: ratio pulled from the runtime param list.
splitRatioTestRuntime :: TestTree
splitRatioTestRuntime = testCase "full [|, param] full, param list = [0.5]" $ do
    let cfg = mkConfig "start = full [|, param] full\n" (v "start") screen 100
        layout = compile cfg
        (ps, left) = layout [0, 1] [0.25]
    assertEqual
        "2 windows: 25/75 vertical split from param list"
        [(0, Rect 0 0 200 600), (1, Rect 200 0 600 600)]
        ps
    assertEqual "no leftover" [] left

-- Split with `param` but no runtime params given: resolveParam logs a
-- warning and falls back to 0.5. The layout still produces the fallback
-- split — we check both the placements and that a log message was emitted.
splitRatioTestEmpty :: TestTree
splitRatioTestEmpty = testCase "full [|, param] full, param list = []" $ do
    let cfg = mkConfig "start = full [|, param] full\n" (v "start") screen 100
        (mLayout, logs) = compileConfig cfg
    layout <- case mLayout of
        Just f -> pure f
        Nothing -> assertFailure "evalRules returned Nothing"
    let (ps, left) = layout [0, 1] []
    assertEqual
        "2 windows: fallback to 50/50 split"
        [(0, Rect 0 0 400 600), (1, Rect 400 0 400 600)]
        ps
    assertEqual "no leftover" [] left
    -- `logs` comes from the compile step, before the layout runs. The
    -- warning we want is emitted during layout, which discards its logs
    -- (per the current design). So we can only check compile-time logs
    -- are clean here — the runtime warning isn't observable.
    assertEqual "no compile-time logs" [] logs

-- splitRule :: TestTree
-- splitRule = testCase "start = full [|] full" $ do
--     let cfg = mkConfig "start = full [|] full" (v "start") screen 100
--         (mLayout, logs) = compileConfig cfg
--     layout <- case mLayout of
--         Just f -> pure f
--         Nothing -> assertFailure "evalRules returned Nothing"
--     let (ps, left) = layout [0, 1] []
--     assertEqual
--         "2 windows: fallback to 50/50 split"
--         [(0, Rect 0 0 400 600), (1, Rect 400 0 400 600)]
--         ps
--     assertEqual "no leftover" [] left
--     -- `logs` comes from the compile step, before the layout runs. The
--     -- warning we want is emitted during layout, which discards its logs
--     -- (per the current design). So we can only check compile-time logs
--     -- are clean here — the runtime warning isn't observable.
--     assertEqual "no compile-time logs" [] logs

-- Test 5: the real-world JSON example
--   snippet  : start = full [|, param] stack
--              stack = full (-) stack
--   screen   : 963 x 1158
--   params   : []          (param keyword defaults to 0.5)
--   windows  : [726678688, 726686416, 726805840]
--
-- Expected placements (verified against the CLI):
--   726678688 -> Rect   0   0  482 1158  (left column, 50 % width)
--   726686416 -> Rect 482   0  481  579  (top-right)
--   726805840 -> Rect 482 579  481  579  (bottom-right)
layoutTest5 :: TestTree
layoutTest5 = testCase "5: param split + recursive horizontal divide (real-world)" $ do
    let src =
            "start = full [|, param] stack\n\
            \stack = full (-) stack\n"
        sd     = ScreenDimension 963 1158
        cfg    = mkConfig src (v "start") sd 25
        layout = compile cfg
        wins   = [726678688, 726686416, 726805840]
        (ps, leftover) = layout wins []

    assertEqual "no leftover windows" [] leftover
    assertEqual
        "3 windows placed correctly"
        [ (726678688, Rect   0   0  482 1158)
        , (726686416, Rect 482   0  481  579)
        , (726805840, Rect 482 579  481  579)
        ]
        ps

leftMaster :: TestTree
leftMaster = testCase "Testing left master" $ do
    let cfg = mkConfig "start = ?stack [|] full\nstack = full (-) ?stack" (v "start") screen 100
        (mLayout, _) = compileConfig cfg
    layout <- case mLayout of
        Just f -> pure f
        Nothing -> assertFailure "evalRules returned Nothing"

    let (ps1, left1) = layout [0] []
    assertEqual
        "1 window: full screen"
        [(0, Rect 0 0 800 600)]
        ps1
    assertEqual "no leftover" [] left1

    let (ps2, left2) = layout [0, 1] []
    assertEqual
        "2 windows"
        [(0, Rect 400 0 400 600), (1, Rect 0 0 400 600)]
        ps2
    assertEqual "no leftover" [] left2

    let (ps3, left3) = layout [0, 1, 2] []
    assertEqual
        "3 windows"
        [(0, Rect 400 0 400 600), (1, Rect 0 0 400 300), (2, Rect 0 300 400 300)]
        ps3
    assertEqual "no leftover" [] left3

rightMaster :: TestTree
rightMaster = testCase "Testing left master" $ do
    let cfg = mkConfig "start = full [|] ?stack\nstack = full (-) ?stack" (v "start") screen 100
        (mLayout, _) = compileConfig cfg
    layout <- case mLayout of
        Just f -> pure f
        Nothing -> assertFailure "evalRules returned Nothing"

    let (ps1, left1) = layout [0] []
    assertEqual
        "1 window: full screen"
        [(0, Rect 0 0 800 600)]
        ps1
    assertEqual "no leftover" [] left1

    let (ps2, left2) = layout [0, 1] []
    assertEqual
        "2 windows"
        [(0, Rect 0 0 400 600), (1, Rect 400 0 400 600)]
        ps2
    assertEqual "no leftover" [] left2

    let (ps3, left3) = layout [0, 1, 2] []
    assertEqual
        "3 windows"
        [(0, Rect 0 0 400 600), (1, Rect 400 0 400 300), (2, Rect 400 300 400 300)]
        ps3
    assertEqual "no leftover" [] left3

layoutTests :: TestTree
layoutTests =
    testGroup
        "Layout Tests"
        [ layoutTest1
        , layoutTest2
        , layoutTest2UnknownStart
        , layoutTest3
        , layoutTest4
        , layoutTest5
        ]

splitRatioTests :: TestTree
splitRatioTests =
    testGroup
        "Split Ratio Tests"
        [ splitRatioTestLiteral
        , splitRatioTestRuntime
        , splitRatioTestEmpty
        ]

optionalTests :: TestTree
optionalTests = 
	testGroup
		"Testing Optinals Behaviour"
		[ leftMaster
		, rightMaster
		]

main :: IO ()
main = defaultMain $ testGroup "Conductor" [layoutTests, splitRatioTests, optionalTests]
