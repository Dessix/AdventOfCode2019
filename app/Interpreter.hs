{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Interpreter where

import Control.Monad (when, unless)
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Freer.Error
import Control.Monad.Freer.State

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Function ((&))

import Text.Printf

import Resultant

data Interpreter i where
    ReadMemory :: Int -> Interpreter Int
    WriteMemory :: Int -> Int -> Interpreter ()
    Input :: Interpreter Int
    Output :: Int -> Interpreter ()
    SetExitCode :: Int -> Interpreter ()

-- makeEffect ''Interpreter

readMemory' :: Member Interpreter effs => Int -> Eff effs Int
readMemory' addr = send $ ReadMemory addr

writeMemory' :: Member Interpreter effs => Int -> Int -> Eff effs ()
writeMemory' addr value = send $ WriteMemory addr value

input' :: Member Interpreter effs => Eff effs Int
input' = send $ Input

output' :: Member Interpreter effs => Int -> Eff effs ()
output' text = send $ Output text

setExitCode' :: Member Interpreter effs => Int -> Eff effs ()
setExitCode' exitCode = send $ SetExitCode exitCode

-- -----------------------------------------------------------------------


-- runInterpreterIO :: LastMember IO i => Eff (Interpreter ': i) ~> Eff i
-- runInterpreterIO = interpretM $ \instr -> case instr of
--     ReadMemory a -> 

type MemIState = ([Int], [Int], Vector Int)

go :: (Members '[ Error String, Resultant [Int], State MemIState ] effs) => Interpreter i -> Eff effs i
go = \case
    ReadMemory addr -> do
        (_, _, mem) <- get @MemIState
        case mem Vector.!? addr of
            Nothing -> throwError @String $ printf "Out of bounds memory access at address %d" addr
            Just v -> pure v
    WriteMemory addr value -> do
        (i, o, mem) <- get @MemIState
        if addr < 0 || addr > (Vector.length mem) then
            throwError @String $ printf "Out of bounds memory write at address %d" addr
        else
            put (i, o, mem Vector.// [(addr, value)])
    Input -> do
        (i, o, mem) <- get @MemIState
        case i of
            x : xs -> do
                put (xs, o, mem)
                pure x
            [] -> throwError "Insufficient inputs available for requested run"
    Output item -> do
        (i, o, mem) <- get @MemIState
        put (i, item : o, mem)
    SetExitCode exitCode -> do
        result [exitCode]


runInterpreterInMemory :: [Int] -> [Int] -> Eff '[Interpreter] i -> (MemIState, Either String (Maybe Int))
runInterpreterInMemory program inputs req =
    let
        result =
            reinterpret3 go req
            & runError @String
            & runResultant @[Int]
            & runState (inputs, [], Vector.fromList program)
            & run
    in
    case result of
        ((Left errorMessage, _), fullState) -> (fullState, Left errorMessage)
        ((Right _, result), (inputs', outputs', state)) ->
            ((inputs', outputs', state), Right (case result of { x : _ -> Just x ; [] -> Nothing }))

readMemoryEach' :: (Member Interpreter r) => [Int] -> Eff r [Int]
readMemoryEach' positions = Prelude.mapM readMemory' positions

readMemorySequence' :: (Member Interpreter r) => Int -> Int -> Eff r [Int]
readMemorySequence' start count = readMemoryEach' [start..start + count - 1]
