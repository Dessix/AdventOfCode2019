{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
module Interpreter where

import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.State

import Control.Arrow ((>>>))

import Data.List
import qualified Data.Text as T
import Data.Vector.Mutable (MVector, IOVector)
import qualified Data.Vector.Mutable as MVector
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Text.Printf
import Debug.Trace

import Utils

data Interpreter m i where
  ReadMemory :: Int -> Interpreter m Int
  WriteMemory :: Int -> Int -> Interpreter m ()
  ConsoleInput :: Interpreter m Int
  ConsoleOutput :: Int -> Interpreter m ()

makeSem ''Interpreter

readMemory' :: Member Interpreter effs => Int -> Sem effs Int
readMemory' addr = readMemory addr

writeMemory' :: Member Interpreter effs => Int -> Int -> Sem effs ()
writeMemory' addr value = writeMemory addr value

input' :: Member Interpreter effs => Sem effs Int
input' = consoleInput

output' :: Member Interpreter effs => Int -> Sem effs ()
output' text = consoleOutput text

readMemoryEach' :: (Member Interpreter r) => [Int] -> Sem r [Int]
readMemoryEach' positions = Prelude.mapM readMemory' positions

readMemorySequence' :: (Member Interpreter r) => Int -> Int -> Sem r [Int]
readMemorySequence' start count = readMemoryEach' [start..start + count - 1]

type MemIState = ([Int], [Int], Vector Int)
type MemIOState = ([Int], [Int], IOVector Int)

-- interpretInstruction :: Interpreter m i 
-- interpretInstruction instruction state =

buildInterpreterInitialState :: [Int] -> [Int] -> MemIState
buildInterpreterInitialState program inputs = (inputs, [], Vector.fromList program)

buildInterpreterInitialStateIO :: [Int] -> [Int] -> IO MemIOState
buildInterpreterInitialStateIO program inputs = do
  programVector <- listToMVector program
  return (inputs, [], programVector)

resumeInterpreterInMemoryInternal :: MemIState -> Sem (Interpreter ': r) a -> Sem r (MemIState, Either String a)
resumeInterpreterInMemoryInternal initialState =
  runState initialState
  . runError
  . reinterpret2 \case
    ReadMemory addr -> do
      (_, _, mem) <- get @MemIState
      case mem Vector.!? addr of
        Nothing -> throw @String $ printf "Out of bounds memory access at address %d" addr
        Just v -> pure v
    WriteMemory addr value -> do
      (i, o, mem) <- get @MemIState
      if addr < 0 || addr > (Vector.length mem) then
        throw @String $ printf "Out of bounds memory write at address %d" addr
      else
        put (i, o, mem Vector.// [(addr, value)])
    ConsoleInput -> do
      (i, o, mem) <- get @MemIState
      case i of
        x : xs -> do
          put (xs, o, mem)
          pure x
        [] -> throw "Insufficient inputs available for requested run"
    ConsoleOutput item -> do
      (i, o, mem) <- get @MemIState
      put (i, item : o, mem)

-- runInterpreterInMemoryInternal :: [Int] -> Sem '[Interpreter] a -> Sem '[] (MemIState, a)
runInterpreterInMemoryInternal :: [Int] -> [Int] -> Sem (Interpreter ': r) a -> Sem r (MemIState, Either String a)
runInterpreterInMemoryInternal program inputs =
  resumeInterpreterInMemoryInternal $ buildInterpreterInitialState program inputs

runInterpreterInMemory :: [Int] -> [Int] -> Sem '[Interpreter] a -> (MemIState, Either String a)
runInterpreterInMemory program input =
  run . runInterpreterInMemoryInternal program input

resumeInterpreterInMemory :: MemIState -> Sem '[Interpreter] a -> (MemIState, Either String a)
resumeInterpreterInMemory state =
  run . resumeInterpreterInMemoryInternal state


resumeInterpreterIO :: (Member (Embed IO) r) => MemIOState -> Sem (Interpreter ': r) a -> Sem r (MemIOState, Either String a)
resumeInterpreterIO initialState =
  runState @MemIOState initialState
  . runError
  . reinterpret2 \case
    ReadMemory addr -> do
      (_, _, mem) <- get @MemIOState
      if addr < 0 || MVector.length mem <= addr then
        throw @String $ printf "Out of bounds memory access at address %d" addr
      else
        embed $ MVector.read mem addr
    WriteMemory addr value -> do
      (i, o, mem) <- get @MemIOState
      if addr < 0 || addr > (MVector.length mem) then
        throw @String $ printf "Out of bounds memory write at address %d" addr
      else do
        embed $ MVector.write mem addr value
        put @MemIOState (i, o, mem)
    ConsoleInput -> do
      (i, o, mem) <- get @MemIOState
      case i of
        x : xs -> do
          put @MemIOState (xs, o, mem)
          pure x
        [] -> throw "Insufficient inputs available for requested run"
    ConsoleOutput item -> do
      (i, o, mem) <- get @MemIOState
      put (i, item : o, mem)
