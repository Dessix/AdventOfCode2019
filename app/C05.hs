{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PartialTypeSignatures #-}
module C05 where

import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Freer.Error
import Control.Monad.Freer.State

import qualified Control.Monad.Fail

import Data.List
import qualified Data.Text as T
import Data.Vector (Vector)
import Data.Vector

import Text.Printf
import Debug.Trace

import Utils

import Resultant



data Interpreter i where
    ReadMemory :: Int -> Interpreter Int
    WriteMemory :: Int -> Int -> Interpreter ()
    Input :: Interpreter Int
    Output :: Int -> Interpreter ()
-- makeEffect ''Interpreter

readMemory' :: Member Interpreter effs => Int -> Eff effs Int
readMemory' addr = send $ ReadMemory addr

writeMemory' :: Member Interpreter effs => Int -> Int -> Eff effs ()
writeMemory' addr value = send $ WriteMemory addr value

input' :: Member Interpreter effs => Eff effs Int
input' = send $ Input

output' :: Member Interpreter effs => Int -> Eff effs ()
output' text = send $ Output text

-- runInterpreterIO :: LastMember IO i => Eff (Interpreter ': i) ~> Eff i
-- runInterpreterIO = interpretM $ \instr -> case instr of
--     ReadMemory a -> 

-- runInterpreterInMemory :: [Int] -> Eff '[Interpreter] effs -> [Int]
-- runInterpreterInMemory inputs req = traceShow (result, log) result
--     where
--         ((Right _, result), log) = run (runWriter (runState inputs (runError (reinterpret3 go req))))
--         go :: Interpreter i -> Eff '[Error (), State [Int], Writer [Int]] i
--         go = \case
--             ReadMemory addr -> do
--                 memory <- get
--                 -- traceM (show (head memory))
--                 tell [6 :: Int]
--                 pure ((head memory) + 2)
--             WriteMemory addr value -> undefined


type MemIState = ([Int], [Int], Vector Int)

go :: (Members '[ Resultant [Int], State MemIState ] effs) => Interpreter i -> Eff effs i
go = \case
    ReadMemory addr -> do
        (_, _, mem) <- (get @MemIState)
        let
            v = mem ! addr
            in do
            pure v
    WriteMemory addr value -> do
        (i, o, mem) <- (get @MemIState)
        put (i, o, mem // [(addr, value)])
    Input -> do
        (i, o, mem) <- (get @MemIState)
        case i of
            x : xs -> do
                put (xs, o, mem)
                pure x
            [] -> error "Insufficient inputs available for requested run"
    Output item -> do
        (i, o, mem) <- (get @MemIState)
        put (i, item : o, mem)

runInterpreterInMemory :: [Int] -> [Int] -> Eff '[Interpreter] i -> (MemIState, Maybe Int)
runInterpreterInMemory program inputs req =
    ((inputs', outputs', state), (case result of { x : _ -> Just x ; [] -> Nothing }))
    where
        ((_, result), (inputs', outputs', state)) =
            run $ runState
                (inputs, [], Data.Vector.fromList program)
                (runResultant $ reinterpret2 go req)


readMemoryEach' :: (Member Interpreter r) => [Int] -> Eff r [Int]
readMemoryEach' positions = Prelude.mapM readMemory' positions

readMemorySequence' :: (Member Interpreter r) => Int -> Int -> Eff r [Int]
readMemorySequence' start count = readMemoryEach' [start..start + count - 1]


runInterpreterAtPosition :: Member Interpreter r => Int -> Eff r (Maybe ())
runInterpreterAtPosition pc =
    --let positionString = (printf "% 8d |" pc) :: String in
    do
    opcode <- readMemory' pc
    case opcode of
        1 -> -- from x, y, write sum to z
            do
            ~[xp, yp, zp] <- readMemorySequence' (succ pc) 3
            destVals <- readMemoryEach' [xp, yp] -- TODO: Bounds check variant
            case Just destVals of
                Nothing -> return Nothing --error "Boundscheck caught"
                Just [x', y'] -> do
                    let z' = x' + y' in do
                        writeMemory' zp z'
                        -- traceM $ printf "%s add %10d @ %-5d %10d @ %-5d -> %10d @ %-5d" pc x' xp y' yp z' zp
                    runInterpreterAtPosition (pc + 4)
        2 -> -- from x, y, write product to z
            do
            ~[xp, yp, zp] <- readMemorySequence' (succ pc) 3
            destVals <- readMemoryEach' [xp, yp] -- TODO: Bounds check variant
            case Just destVals of
                Nothing -> return Nothing --error "Boundscheck caught"
                Just [x', y'] ->
                    let z' = x' * y' in do
                        writeMemory' zp z'
                        --traceM $ printf "%s mul %10d @ %-5d %10d @ %-5d -> %10d @ %-5d" positionString x' xp y' yp z' zp
                        runInterpreterAtPosition (pc + 4)
        99 -> -- Bail out
            do
            --traceM $ printf "%s hcf" positionString
            return $ Just ()
        unknown -> -- Unknown opcode
            error (printf "Opcode %d is not implemented" unknown)


