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
module PInterp where

import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.State

import Control.Arrow ((>>>))

import Data.List
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Text.Printf
import Debug.Trace

data Interpreter m i where
  ReadMemory :: Int -> Interpreter m Int
  WriteMemory :: Int -> Int -> Interpreter m ()
  ConsoleInput :: Interpreter m Int
  ConsoleOutput :: Int -> Interpreter m ()

makeSem ''Interpreter

type MemIState = ([Int], [Int], Vector Int)


-- runAdventPure :: [Int] -> Sem '[Interpreter] a -> Sem '[] (MemIState, a)
runAdventPure :: [Int] -> [Int] -> Sem (Interpreter ': r) a -> Sem r (MemIState, Either String a)
runAdventPure program inputs =
  runState (inputs, [], Vector.fromList program)
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


runAdventPureComplete :: [Int] -> [Int] -> Sem '[Interpreter] a -> (MemIState, Either String a)
runAdventPureComplete program input =
  run . runAdventPure program input

-- testRun = run $ runLastWriter @Int $ evalState ([1,2,3,4]::[Int]) (runProgram adventPure prog)
-- testRunIO = runLift $ runLastWriter @Int $ evalState ([1,2,3,4]::[Int]) (runProgram adventIO prog)
