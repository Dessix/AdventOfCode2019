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
import Polysemy
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

runOp :: Member Interpreter r => Int -> Int -> IntOp -> Sem r (Maybe Int)
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

runInterpreterAtPositionYieldingWithDebugName :: Member Interpreter r => Maybe String -> Int -> Sem r (Maybe Int)
runInterpreterAtPositionYieldingWithDebugName debugName pc =
    let
        getParam :: Member Interpreter r => ParameterMode -> Int -> Sem r Int
        getParam PositionMode position = readMemory' position >>= readMemory'
        getParam ImmediateMode position = readMemory' position
        getParam WritePseudoMode position = getParam ImmediateMode position
        positionString = (printf "% 8d |" pc) :: String
    in do
    opCode <- readMemory' pc
    (op, paramCount) <- buildOp getParam $ parseOpInfo opCode pc

    case debugName of
        Just "" -> traceM (printf "%5d | %s" pc (show op))
        Just name -> traceM (printf "%s | %5d | %s" name pc (show op))
        _ -> return ()
    maybeNextPosition <- runOp pc (fromIntegral paramCount) op
    case maybeNextPosition of
        Nothing -> return $ Nothing
        Just nextPosition -> return $ Just nextPosition

runInterpreterAtPositionYielding :: Member Interpreter r => Bool -> Int -> Sem r (Maybe Int)
runInterpreterAtPositionYielding debug pc =
    runInterpreterAtPositionYieldingWithDebugName (if debug then (Just "") else Nothing) pc

runInterpreterAtPosition :: Member Interpreter r => Bool -> Int -> Sem r ()
runInterpreterAtPosition debug pc = do
    res <- runInterpreterAtPositionYielding debug pc
    case res of
        Just next -> runInterpreterAtPosition debug next
        Nothing -> return ()


-- "Machines"

--- Run a single machine one step; exceptions (such as from insufficient inputs) undo the execution
runSingleMachine :: Maybe String -> (MemIState, Either Int Bool) -> (MemIState, Either Int Bool)
runSingleMachine debugName cur@(_, Right _) = cur
runSingleMachine debugName cur@(state, Left nextPos) =
    case resumeInterpreterInMemory state $ do runInterpreterAtPositionYieldingWithDebugName debugName nextPos of
        (state', Right (Just nextPos')) -> (state', Left nextPos')
        (state', Right Nothing) -> (state', Right True)
        (_, Left _) -> case cur of -- An error occurred; handle it
            (((_ : _), _, _), _) -> (state, Right False) -- Had input available; was not a lack-of-input failure; bail
            _ -> cur -- No input available; may have been lack-of-input, continue next cycle

-- Runs a machine, producing a new state and outputs to feed to the next
runMachineStep :: Maybe String -> (MemIState, Either Int Bool) -> ((MemIState, Either Int Bool), [Int])
runMachineStep debugName cur@(_, Right _) = (cur, [])
runMachineStep debugName ((i, o, state), Left nextPos) =
    let ((i', o', state'), result) = runSingleMachine debugName ((i, [], state), Left nextPos)
    in (((i', o ++ o', state'), result), o')


foldMachineSet ::
    [(Int, (MemIState, Either Int Bool))]
    -> [(Int, (MemIState, [Int], Either Int Bool))]
foldMachineSet [] = []
foldMachineSet ((name, x) : []) = let ((s, r), o) = runMachineStep (Just $ show name) x in [(name, (s, o, r))]
foldMachineSet ((name, x) : (nextName, ((nextMInputs, nextMOutputs, nextMState), nextMResult)) : xs) =
    let ((s, r), o) = runMachineStep (Just $ show name) x in
    let rest = foldMachineSet ((nextName, ((nextMInputs ++ o, nextMOutputs, nextMState), nextMResult)) : xs) in
    (name, (s, o, r)) : rest

-- Run machines in provided state-stack, returning their new states
-- newly-emitted outputs are forwarded into the states of the next machine, or returned at the end
-- For each machine, (state, this-run outputs, Either NextPosition or Result)
-- Current always gets appended to the list; convert to dequeue when algorithm is stable
runMachineSet ::
    [(MemIState, Either Int Bool)]
    -> [(MemIState, [Int], Either Int Bool)]
runMachineSet machines = map snd $ foldMachineSet $ zip [0..] machines

buildSequencerInitialStates' :: [Int] -> [[Int]] -> [MemIState]
buildSequencerInitialStates' program [] = []
buildSequencerInitialStates' program initialInputs =
    map (buildInterpreterInitialState program) initialInputs

-- Builds interpreters wherein the initial inputs are set to the given values
-- The starter set is added to the first interpreter after its passed-in initial inputs
buildSequencerInitialStates :: [Int] -> [[Int]] -> [Int] -> [MemIState]
buildSequencerInitialStates program [] _ = []
buildSequencerInitialStates program (firstInitial : restInitials) starterInputs =
    buildSequencerInitialStates' program ((firstInitial ++ starterInputs) : restInitials)

-- Builds machine initial states where each starts at position 0 with the given inputs and expects outputs to feed forward
buildSequencerMachines :: [Int] -> [[Int]] -> [Int] -> [(MemIState, Either Int Bool)]
buildSequencerMachines program initialInputs starterInputs =
    map (\x->(x, Left 0)) $ buildSequencerInitialStates program initialInputs starterInputs


runMachineSequencerUntil ::
    ([(MemIState, [Int], Either Int Bool)] -> [(MemIState, [Int], Either Int Bool)])
    -> ([(MemIState, [Int], Either Int Bool)] -> Bool)
    -> [(MemIState, Either Int Bool)]
    -> [(MemIState, Either Int Bool)]
runMachineSequencerUntil sequencer predicate [] = []
runMachineSequencerUntil sequencer predicate machines =
    let
        res = runMachineSet machines
        res' = map (\(s, o, r) -> (s, r)) $ sequencer res
    in if predicate res then res' else runMachineSequencerUntil sequencer predicate res'

sequenceMachinesDirectionallyUntil :: ([(MemIState, [Int], Either Int Bool)] -> Bool) -> [(MemIState, Either Int Bool)] -> [(MemIState, Either Int Bool)]
sequenceMachinesDirectionallyUntil predicate machines = runMachineSequencerUntil id predicate machines

sequenceMachinesCyclicallyUntil :: ([(MemIState, [Int], Either Int Bool)] -> Bool) -> [(MemIState, Either Int Bool)] -> [(MemIState, Either Int Bool)]
sequenceMachinesCyclicallyUntil predicate machines =
    let
        selectNewOutputs (_, o', _) = o'
        addNewInputs ((i, o, s), o', r) i' = ((i ++ i', o, s), o', r)
        sequencer :: [(MemIState, [Int], Either Int Bool)] -> [(MemIState, [Int], Either Int Bool)]
        sequencer items = headApply (\x -> addNewInputs x (selectNewOutputs (last items))) items
    in runMachineSequencerUntil sequencer predicate machines

-- Usages of the above sequencers

sequenceMachinesDirectionallyUntilFirstOutput machines =
    sequenceMachinesDirectionallyUntil (\machines -> case (last machines) of (_, (_ : _), _) -> True ; _ -> False) machines

sequenceMachinesCyclicallyUntilFirstOutput machines =
    sequenceMachinesCyclicallyUntil (\machines -> case (last machines) of (_, (_ : _), _) -> True ; _ -> False) machines

sequenceMachinesDirectionallyUntilLastExits machines =
    sequenceMachinesDirectionallyUntil (\machines -> case (last machines) of (_, _, Right _) -> True ; _ -> False) machines

sequenceMachinesCyclicallyUntilLastExits machines =
    sequenceMachinesCyclicallyUntil (\machines -> case (last machines) of (_, _, Right _) -> True ; _ -> False) machines

