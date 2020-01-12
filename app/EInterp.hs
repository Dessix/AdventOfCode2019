{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
-- {-# LANGUAGE UndecidableInstances #-}
module EInterp where

-- https://hackage.haskell.org/package/extensible-effects-5.0.0.1/docs/src/Control.Eff.Operational.Example.html#Jail

import Control.Eff
import Control.Eff.Reader.Lazy
import Control.Eff.Writer.Lazy
import Control.Eff.State.Lazy
import Control.Eff.Exception
import Control.Eff.Operational
import Control.Eff.Extend

import Data.Function (fix)
import Data.List
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Text.Printf
import Debug.Trace

-- data Interpreter i where
--     ReadMemory :: Int -> Interpreter Int

-- runInterpreterInMemory :: (Member (State [Int]) r) => Interpreter i -> Eff r i
-- runInterpreterInMemory (ReadMemory addr) = do
--     mem <- get
--     return (mem !! addr) 

-- runInterpreterInMemory' :: [Int] -> Eff '[Interpreter] effs -> [Int]
-- runInterpreterInMemory' inputs req = traceShow (result) result
--     where
--         (_, result) = run (runState inputs (go ))
--         go :: Interpreter i -> Eff '[State [Int]] i
--         go = \case
--             ReadMemory addr -> do
--                 let v = 12 in do
--                     traceM ("Value at trace: " ++ (show v))
--                     pure v
--             --WriteMemory addr value -> undefined


data Interpreter i where
    ReadMemory :: Int -> Interpreter Int
    WriteMemory :: Int -> Int -> Interpreter ()
    Input :: Interpreter Int
    Output :: Int -> Interpreter ()
    SetExitCode :: Int -> Interpreter Int

-- makeEffect ''Interpreter

-- readMemory' :: Member Interpreter effs => Int -> Eff effs Int
-- readMemory' addr = send $ ReadMemory addr

-- writeMemory' :: Member Interpreter effs => Int -> Int -> Eff effs ()
-- writeMemory' addr value = send $ WriteMemory addr value

-- input' :: Member Interpreter effs => Eff effs Int
-- input' = send $ Input

-- output' :: Member Interpreter effs => Int -> Eff effs ()
-- output' text = send $ Output text

-- setExitCode' :: Member Interpreter effs => Int -> Eff effs Int
-- setExitCode' exitCode = send $ SetExitCode exitCode


prog :: Member (Program Interpreter) r => Eff r Int
prog = do
   singleton $ WriteMemory 0 99
   readValue <- singleton $ ReadMemory 4
   singleton $ WriteMemory 1 0
   singleton $ WriteMemory 2 readValue
   return 9

-- | Then, implements interpreters from the data to effects.
adventIO :: Lifted IO r => Interpreter a -> Eff r a
adventIO (WriteMemory addr v) = lift $ putStrLn (printf "%d @ %d" addr v)
adventIO (ReadMemory addr) = lift (do l <- getLine; return (read l))

adventPure :: [ Writer Int, State [Int] ] <:: r => Interpreter a -> Eff r a
adventPure (WriteMemory addr v) = tell (v)
adventPure (ReadMemory addr) = do
  x <- get
  case x of
    [] -> return (-1)
    y:ys -> put ys >> return y

type MemIState = ([Int], [Int], Vector Int)

withInterpreter :: Monad m => a -> s -> m (a, s)
withInterpreter x s = return (x, s)



runInterpreter :: Eff (Interpreter ': r) a -> Eff r (a, MemIState)
runInterpreter m = undefined --handle_relay' interpreterHandler withInterpreter) m


--runAdventPure :: Eff '[Interpreter] i -> Eff '[] (i, MemIState)
runAdventPure :: Eff (Interpreter ': r) a -> Eff r (a, MemIState)
runAdventPure input = undefined

runAdventPureComplete :: Eff '[Interpreter] i -> (i, MemIState)
runAdventPureComplete input = run $ runAdventPure input

testRun = run $ runLastWriter @Int $ evalState ([1,2,3,4]::[Int]) (runProgram adventPure prog)

testRunIO = runLift $ runLastWriter @Int $ evalState ([1,2,3,4]::[Int]) (runProgram adventIO prog)