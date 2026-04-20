{-# LANGUAGE OverloadedStrings #-}

module Conductor (
    parseTest,
    runTests,
    evalRules,
    Config (..),
    Rect (..),
    ScreenDimension (..),
    WindowLayoutFunc,
    WindowTransform,
) where

import Conductor.Parser hiding (params, x, y)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Text.Megaparsec (errorBundlePretty, parse)

parseTest :: Text -> IO ()
parseTest input = case parse parseConductor "" input of
    Left err -> putStrLn $ "Error: " ++ errorBundlePretty err
    Right result -> BL.putStrLn (encode result)

runTests :: IO ()
runTests = do
    putStrLn "Testing parser..."
    parseTest "start = full [|] none\n"
    parseTest "end = none [-] full\n"
    parseTest "start = (full [|] full) [-] none\n"
    parseTest "start = full [|] ?stack\nstack = full (-) ?stack"

data ScreenDimension = ScreenDimension
    { sdWidth :: Int
    , sdHeight :: Int
    }
    deriving (Show, Eq)

type WindowId = Int

data Rect = Rect
    { rX :: Int
    , rY :: Int
    , rW :: Int
    , rH :: Int
    }
    deriving (Show, Eq)

type WindowTransform = Rect

type Placements = [(WindowId, WindowTransform)]

type WindowLayoutFunc = [WindowId] -> [Float] -> (Placements, [WindowId])

data Config = Config
    { cStartVariable :: Variable
    , cRuleMap :: Map Variable Expression
    , cScreenDimension :: ScreenDimension
    , cMaxDepth :: Int
    }
    deriving (Show)

data EvalState = EvalState
    { eDepth :: Int
    , eRect :: Rect
    , eWins :: [WindowId]
    , eParams :: [Float]
    }
    deriving (Show)

type EvalMonad a = ReaderT Config (Writer [String]) a

runEval :: Config -> EvalMonad a -> (a, [String])
runEval cfg m = runWriter (runReaderT m cfg)

logMsg :: String -> EvalMonad ()
logMsg s = lift (tell [s])

-- Entry point
evalRules :: EvalMonad (Maybe WindowLayoutFunc)
evalRules = do
    cfg <- ask
    let sd = cScreenDimension cfg
        rect = Rect 0 0 (sdWidth sd) (sdHeight sd)
        startExpr = M.lookup (cStartVariable cfg) (cRuleMap cfg)
    case startExpr of
        Nothing -> do
            logMsg "eval: Unknown start variable"
            return Nothing
        Just r -> return . Just $ \wins params ->
            let initSt = EvalState 0 rect wins params
                ((placed, stFinal), _logs) = runEval cfg (evalExpr r initSt)
             in (placed, eWins stFinal)

-- Implicit-? resolution
normalizeOpts :: Maybe Opt -> Maybe Opt -> (Maybe Opt, Maybe Opt)
normalizeOpts Nothing Nothing = (Nothing, Just Opt)
normalizeOpts l r = (l, r)

-- Demand: how many placements would this subtree produce, given
-- `avail` windows. Used by Divide (for slice count) and by ?-collapse
-- (to detect the "produces zero placements" trigger).
demand :: Expression -> Int -> Int -> EvalMonad Int
demand expr depth avail = do
    cfg <- ask
    if depth > cMaxDepth cfg
        then do
            logMsg $ "demand: depth cap at " ++ show depth
            return 0
        else case expr of
            ExprUnary op -> demandOperand op depth avail
            ExprBinary lOpt left operator rOpt right -> do
                let (lOpt', rOpt') = normalizeOpts lOpt rOpt
                dL <- demandOperand left depth avail
                dR <- demandOperand right depth (max 0 (avail - dL))
                case operator of
                    Divide _ -> return (dL + dR)
                    _ -> case (lOpt', rOpt') of
                        (Just _, _) | dL == 0 -> return dR
                        (_, Just _) | dR == 0 -> return dL
                        _ -> return (dL + dR)

demandOperand :: Operand -> Int -> Int -> EvalMonad Int
demandOperand op depth avail
    | avail <= 0 = return 0
    | otherwise = case op of
        OperandLit Full -> return 1
        OperandLit None -> return 0
        OperandParen e -> demand e depth avail
        OperandVar v -> do
            mExpr <- asks (M.lookup v . cRuleMap)
            case mExpr of
                Nothing -> do
                    logMsg $ "demandOperand: unknown variable " ++ show v
                    return 0
                Just r -> demand r (depth + 1) avail

-- Evaluator
evalExpr :: Expression -> EvalState -> EvalMonad (Placements, EvalState)
evalExpr expr st = do
    cfg <- ask
    if eDepth st > cMaxDepth cfg
        then do
            logMsg $ "evalExpr: depth cap at " ++ show (eDepth st)
            return ([], st)
        else case expr of
            ExprUnary op -> evalOperand op st
            ExprBinary lOptRaw left operator rOptRaw right -> do
                let (lOpt, rOpt) = normalizeOpts lOptRaw rOptRaw
                    d = eDepth st
                    avail = length (eWins st)
                dL <- demandOperand left d avail
                dR <- demandOperand right d (max 0 (avail - dL))
                case (lOpt, rOpt) of
                    (Just _, _) | dL == 0 -> evalOperand right st
                    (_, Just _) | dR == 0 -> evalOperand left st
                    _ -> evalBinary operator left right st dL dR

evalBinary :: Operator -> Operand -> Operand -> EvalState -> Int -> Int -> EvalMonad (Placements, EvalState)
evalBinary operator left right st dL dR =
    case operator of
        Split dir mParam -> do
            (ratio, params') <- resolveRatio mParam (eParams st)
            let (rectL, rectR) = splitRect dir ratio (eRect st)
                stL = st{eRect = rectL, eParams = params'}
            (pL, st1) <- evalOperand left stL
            let stR = st1{eRect = rectR}
            (pR, st2) <- evalOperand right stR
            return (pL ++ pR, st2{eRect = eRect st})
        Divide dir -> do
            let total = dL + dR
            if total <= 0
                then return ([], st)
                else do
                    let (rectL, rectR) = divideRect dir dL total (eRect st)
                        stL = st{eRect = rectL}
                    (pL, st1) <- evalOperand left stL
                    let stR = st1{eRect = rectR}
                    (pR, st2) <- evalOperand right stR
                    return (pL ++ pR, st2{eRect = eRect st})
        Layer spec -> do
            (rectL, rectR, params') <- resolveLayer spec (eRect st) (eParams st)
            let stL = st{eRect = rectL, eParams = params'}
            (pL, st1) <- evalOperand left stL
            let stR = st1{eRect = rectR}
            (pR, st2) <- evalOperand right stR
            return (pL ++ pR, st2{eRect = eRect st})

evalOperand :: Operand -> EvalState -> EvalMonad (Placements, EvalState)
evalOperand op st = case op of
    OperandLit Full -> case eWins st of
        [] -> return ([], st)
        (w : ws) -> return ([(w, eRect st)], st{eWins = ws})
    OperandLit None -> return ([], st)
    OperandParen e -> evalExpr e st
    OperandVar v -> do
        mExpr <- asks (M.lookup v . cRuleMap)
        case mExpr of
            Nothing -> do
                logMsg $ "evalOperand: unknown variable " ++ show v
                return ([], st)
            Just r -> evalExpr r (st{eDepth = eDepth st + 1})

-- Geometry helpers
splitRect :: PartitionDirection -> Float -> Rect -> (Rect, Rect)
splitRect Vertical ratio (Rect x y w h) =
    let wL = round (fromIntegral w * ratio)
     in (Rect x y wL h, Rect (x + wL) y (w - wL) h)
splitRect Horizontal ratio (Rect x y w h) =
    let hL = round (fromIntegral h * ratio)
     in (Rect x y w hL, Rect x (y + hL) w (h - hL))

divideRect :: PartitionDirection -> Int -> Int -> Rect -> (Rect, Rect)
divideRect Vertical dL total (Rect x y w h) =
    let wL = (w * dL) `div` total
     in (Rect x y wL h, Rect (x + wL) y (w - wL) h)
divideRect Horizontal dL total (Rect x y w h) =
    let hL = (h * dL) `div` total
     in (Rect x y w hL, Rect x (y + hL) w (h - hL))

resolveLayer :: LayerSpec -> Rect -> [Float] -> EvalMonad (Rect, Rect, [Float])
resolveLayer spec rect@(Rect x y w h) params = case spec of
    LayerDirection d ->
        let (dx, dy) = case d of
                LayerLeft -> (-w, 0)
                LayerRight -> (w, 0)
                LayerUp -> (0, -h)
                LayerDown -> (0, h)
         in return (rect, Rect (x + dx) (y + dy) w h, params)
    LayerParams px py -> do
        (fx, params1) <- resolveParam px params
        (fy, params2) <- resolveParam py params1
        let dx = round (fromIntegral w * fx)
            dy = round (fromIntegral h * fy)
        return (rect, Rect (x + dx) (y + dy) w h, params2)

resolveRatio :: Maybe Param -> [Float] -> EvalMonad (Float, [Float])
resolveRatio Nothing ps = return (0.5, ps)
resolveRatio (Just p) ps = resolveParam p ps

resolveParam :: Param -> [Float] -> EvalMonad (Float, [Float])
resolveParam (ParamFloat f) ps = return (f, ps)
resolveParam ParamKeyword (p : ps) = return (p, ps)
resolveParam ParamKeyword [] = do
    logMsg "resolveParam: param list exhausted, defaulting to 0.5"
    return (0.5, [])
