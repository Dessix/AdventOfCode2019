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
            | TReadInt
            | TWriteInt
            | TJmpNZ
            | TJmpZ
            | TLessThan
            | TEqual
            | TExit
    deriving (Eq, Show)

data IntOp = Sum2 Int Int WriteAddress
           | Mul2 Int Int WriteAddress
           | ReadInt WriteAddress
           | WriteInt Int
           | JmpNZ Int Int
           | JmpZ Int Int
           | LessThan Int Int WriteAddress
           | Equal Int Int WriteAddress
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
    3 -> (TReadInt, 1, [(0, WritePseudoMode)])
    4 -> (TWriteInt, 1, [])
    5 -> (TJmpNZ, 2, [])
    6 -> (TJmpZ, 2, [])
    7 -> (TLessThan, 3, [(2, WritePseudoMode)])
    8 -> (TEqual, 3, [(2, WritePseudoMode)])
    99 -> (TExit, 0, [])
    unsupported -> error $ "No opType mapping for operation code " ++ (show unsupported)

buildOp :: (Monad m) => (ParameterMode -> Int -> m Int) -> (TIntOp, Arity, [(ParameterMode, Int)]) -> m (IntOp, Arity)
buildOp getParameterM (opType, arity, paramSpecs) = do
    params <- mapM (uncurry getParameterM) paramSpecs
    let op = case opType of
            TSum2 -> let ~[a, b, outAddr] = params in return $ Sum2 a b outAddr
            TMul2 -> let ~[a, b, outAddr] = params in return $ Mul2 a b outAddr
            TReadInt -> let ~[outAddr] = params in return $ ReadInt outAddr
            TWriteInt -> let ~[a] = params in return $ WriteInt a
            TJmpNZ -> let ~[a, b] = params in return $ JmpNZ a b
            TJmpZ -> let ~[a, b] = params in return $ JmpZ a b
            TLessThan -> let ~[a, b, outAddr] = params in return $ LessThan a b outAddr
            TEqual -> let ~[a, b, outAddr] = params in return $ Equal a b outAddr
            TExit -> return Exit
        in do
            op' <- op
            return (op', arity)

runOp :: (Member Interpreter r) => Int -> Int -> IntOp -> Eff r (Maybe Int)
runOp pc arity op =
    let defaultNextPos = Just $ pc + 1 + arity
    in case op of
    (Sum2 a b outAddr) -> do writeMemory' outAddr (a + b); return defaultNextPos
    (Mul2 a b outAddr) -> do writeMemory' outAddr (a * b); return defaultNextPos
    (ReadInt outAddr) -> do v <- input'; writeMemory' outAddr v; return defaultNextPos
    (WriteInt a) -> do output' a; return defaultNextPos
    (JmpNZ a b) -> return $ if a /= 0 then Just b else defaultNextPos
    (JmpZ a b) -> return $ if a == 0 then Just b else defaultNextPos
    (LessThan a b outAddr) -> do writeMemory' outAddr (if a < b then 1 else 0); return defaultNextPos
    (Equal a b outAddr) -> do writeMemory' outAddr (if a == b then 1 else 0); return defaultNextPos
    Exit -> return Nothing


parseOpInfo :: Int -> Int -> (TIntOp, Arity, [(ParameterMode, Int)])
parseOpInfo opCodeWithModes opPosition =
    let
        opCode = opCodeWithModes `mod` 100
        (opType, arity, ModeOverrides modeOverrides) = opCodeToOperation opCode
        arityInt :: Int
        arityInt = fromIntegral arity
        modes :: [ParameterMode]
        modes = List.unfoldr (\memo ->        (digitsRightToLeftUnfolderR memo) >>= (\(a, b) -> Just ((parseParamMode (fromIntegral a)), b))) ((opCodeWithModes - opCode) `div` 100)
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

    when debug $ traceM (printf "%5d | %s" pc (show op))
    maybeNextPosition <- runOp pc (fromIntegral paramCount) op
    case maybeNextPosition of
        Nothing -> return $ Just ()
        Just nextPosition -> runInterpreterAtPosition debug nextPosition
