{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PartialTypeSignatures #-}
module IntCodeInterpreter where

import Control.Monad (when, unless)
import Control.Monad.Freer

import qualified Control.Monad.Fail

import Data.List
import qualified Data.List as List
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Function ((&))

import Text.Printf
import Debug.Trace

import Utils

import Interpreter


type WriteAddress = Int
type Arity = Word
newtype ModeOverrides = ModeOverrides [(Word, ParameterMode)]

data ParameterMode = PositionMode | ImmediateMode | WritePseudoMode
    deriving (Eq, Show)

data TIntOp = TSum2
            | TMul2
            | TExit
    deriving (Eq, Show)

data IntOp = Sum2 Int Int WriteAddress
           | Mul2 Int Int WriteAddress
           | Exit
    deriving (Eq, Show)

parseParamMode :: Int -> ParameterMode
parseParamMode = \case
    0 -> PositionMode
    1 -> ImmediateMode
    unsupported -> error ("Unsupported parameter mode " ++ (show unsupported))

-- opCodeToOperation :: (Monad m) => ((ParameterMode, Int) -> m Int) -> Int -> m IntOp
opCodeToOperation :: Int -> (TIntOp, Arity, ModeOverrides)
opCodeToOperation = (\(op, arity, modes) -> (op, arity, ModeOverrides modes)) . \case
    1 -> (TSum2, 3, [(2, WritePseudoMode)])
    2 -> (TMul2, 3, [(2, WritePseudoMode)])
    99 -> (TExit, 0, [])
    unsupported -> error $ "No opType mapping for operation code " ++ (show unsupported)

buildOp :: (Monad m) => (ParameterMode -> Int -> m Int) -> (TIntOp, Arity, [(ParameterMode, Int)]) -> m (IntOp, Arity)
buildOp getParameterM (opType, arity, paramSpecs) = do
    params <- mapM (uncurry getParameterM) paramSpecs
    let op = case opType of
            TSum2 -> let ~[a, b, outAddr] = params in return $ Sum2 a b outAddr
            TMul2 -> let ~[a, b, outAddr] = params in return $ Mul2 a b outAddr
            TExit -> return Exit
        in do
            op' <- op
            return (op', arity)

runOp :: (Member Interpreter r) => IntOp -> Eff r Bool
runOp (Sum2 a b outAddr) = do writeMemory' outAddr (a + b); return True
runOp (Mul2 a b outAddr) = do writeMemory' outAddr (a * b); return True
runOp Exit = return False


parseOpInfo :: Int -> Int -> (TIntOp, Arity, [(ParameterMode, Int)])
parseOpInfo opCodeWithModes opPosition =
    let
        opCode = opCodeWithModes `mod` 100
        (opType, arity, ModeOverrides modeOverrides) = opCodeToOperation opCode
        arityInt :: Int
        arityInt = fromIntegral arity
        modes :: [ParameterMode]
        modes = List.unfoldr (\memo -> case (digitsRightToLeftUnfolderR memo) of Just (a, b) -> Just ((parseParamMode (fromIntegral a)), b); Nothing -> Nothing) ((opCodeWithModes - opCode) `div` 100)
        modesAtArity = take arityInt $ List.concat [modes, (List.repeat PositionMode)]
        overriddenModes :: Vector ParameterMode
        overriddenModes = (Vector.fromList modesAtArity) Vector.// (map (\(idx, val) -> (fromIntegral idx, val)) modeOverrides)
        parameterPositions = map (+ opPosition) [1..arityInt]
    in
        (opType, arity, (zip (Vector.toList overriddenModes) parameterPositions))

runInterpreterAtPosition :: Member Interpreter r => Bool -> Int -> Eff r (Maybe ())
runInterpreterAtPosition debug pc =
    let
        getParam :: Member Interpreter r => ParameterMode -> Int -> Eff r Int
        getParam PositionMode position = readMemory' position >>= readMemory'
        getParam ImmediateMode position = readMemory' position
        getParam WritePseudoMode position = getParam ImmediateMode position
        positionString = (printf "% 8d |" pc) :: String
    in do
    opCode <- readMemory' pc
    (op, paramCount) <- buildOp getParam $ parseOpInfo opCode pc

    continue <- runOp op
    when debug $ traceM (printf "%5d | %s" pc (show op))
    if continue then runInterpreterAtPosition debug (pc + 1 + (fromIntegral paramCount)) else return $ Just ()