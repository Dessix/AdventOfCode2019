{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PartialTypeSignatures #-}
module C07 where

import Control.Monad (when, unless, replicateM, forM_)
import qualified Control.Monad.Parallel as Par

import qualified Control.Monad.Fail

import Data.Function (on, (&))
import Data.Ord (comparing)
import Data.List
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Data.Vector.Mutable (MVector, IOVector)
import qualified Data.Vector.Mutable as MVector
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Polysemy (runM, raise)
import Polysemy (Sem (..), Member (..))
import Polysemy.Embed (embed, Embed (..))

import Text.Printf
import Debug.Trace

import Control.Exception (assert)
import Control.Monad.ST
import Control.Concurrent hiding (Chan, newChan, readChan, writeChan, getChanContents, writeList2Chan, dupChan)
import Control.Concurrent.Chan.Unagi

import Utils

import Interpreter
import IntCodeInterpreter


_testProgram =
    [
        1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,6,19,23,2,23,6,27,1,5,27,31,
        1,31,9,35,2,10,35,39,1,5,39,43,2,43,10,47,1,47,6,51,2,51,6,55,2,55,13,59,
        2,6,59,63,1,63,5,67,1,6,67,71,2,71,9,75,1,6,75,79,2,13,79,83,1,9,83,87,1,
        87,13,91,2,91,10,95,1,6,95,99,1,99,13,103,1,13,103,107,2,107,10,111,1,9,
        111,115,1,115,10,119,1,5,119,123,1,6,123,127,1,10,127,131,1,2,131,135,1,135,10,0,99,2,14,0,0]

testInterpreter = runInterpreterInMemory _testProgram [] $ do { writeMemory' 1 12; writeMemory' 2 2; runInterpreterAtPositionYielding True 0; return 0 }
testInterpreterIO = do
    initialState <- buildInterpreterInitialStateIO _testProgram []
    ((inputs, outputs, mem), res) <- Polysemy.runM $ resumeInterpreterIO initialState $ do
        writeMemory' 1 12
        writeMemory' 2 2
        runInterpreterAtPositionYielding True 0
        return 0
    memClone <- mVectorToList mem
    return ((inputs, outputs, memClone), res)

runDiagnosticsEngine consoleInputs = do
    program <- getIntsFromConsoleUntilBlank;
    case
        runInterpreterInMemory program consoleInputs $ do
            runInterpreterAtPositionYielding True 0
            return 0
        of
            (_, Left e) -> error (printf "Diagnostic run failed with error: %s" e)
            ((i, o, _), Right res) -> (printf "Diagnostic run succeeded with result %s and outputs: %s" (show res) (show o))


runAmplifier :: Bool -> [Int] -> Int -> Int -> Maybe Int
runAmplifier debug program mode input =
    case
        runInterpreterInMemory program [mode, input] $ do
            runInterpreterAtPositionYielding True 0
            return 0
        of
            (_, Left e) -> Nothing
            ((i, o, _), Right res) -> Just (head o)

runAmplifierStack :: Bool -> [Int] -> [Int] -> Int -> Maybe Int
runAmplifierStack debug program [] input = Just input
runAmplifierStack debug program (mode : modes) input =
    (runAmplifier debug program mode input) >>= (runAmplifierStack debug program modes) 
    -- The above line equates to:
    -- case runAmplifier debug program mode input of
    --     Just output -> runAmplifierStack debug program modes output
    --     Nothing -> Nothing

runAmplifierStackFromConsole :: Bool -> [Int] -> Int -> IO (Maybe Int)
runAmplifierStackFromConsole debug modes initialInput = do
    program <- getIntsFromConsoleUntilBlank
    case runAmplifierStack debug program modes initialInput of
        Nothing -> return Nothing
        Just i -> return $ Just i

runAmplifierStackModesetsFromConsole :: Bool -> [[Int]] -> Int -> IO [([Int], Int)]
runAmplifierStackModesetsFromConsole debug modesets initialInput = do
    program <- getIntsFromConsoleUntilBlank
    return $ Maybe.catMaybes $ map (\modeset ->
            let x = runAmplifierStack debug program modeset initialInput in
                fmap (\i -> (modeset, i)) x
        ) modesets

solveDay7Part1 = do
    solutions <- runAmplifierStackModesetsFromConsole True (List.permutations [0..4]) 0
    return $ maximumBy (compare `on` snd) solutions


findIntCodeTweakWithResult :: [Int] -> Int -> [[(Int, Int)]] -> Maybe ([(Int, Int)], Vector Int)
findIntCodeTweakWithResult _ _ [] = Nothing
findIntCodeTweakWithResult input desired (tweaks : rest) =
    let output = runInterpreterInMemory input [] $ do
            mapM_ (\(pos, v) -> writeMemory' pos v) tweaks
            runInterpreterAtPositionYielding True 0
            resultCode <- readMemory' 0
            return $ Just resultCode
    in case output of
        (_, Left err) -> do
            traceM (printf "Error running with tweaks %s: %s" (show tweaks) err)
            findIntCodeTweakWithResult input desired rest
        (_, Right (Nothing)) -> do
            traceM (printf "Never reached result assignment on tweaks %s" (show tweaks))
            findIntCodeTweakWithResult input desired rest
        ((i, o, state), Right (Just resultCode)) | resultCode == desired -> Just (tweaks, state)
        (_, Right (Just _)) -> do
            traceM (printf "Incorrect result on tweaks %s" (show tweaks))
            findIntCodeTweakWithResult input desired rest


test1202 =
    findIntCodeTweakWithResult [2,13,17,0, 1,0,21,0, 99,0,0,0, 2,100,0,14, 2,0,0,18, 2,0,0,22] 1202 [[(1,400)], [(17,12),(21,2)]]


solveDay2Part2 requestedNumber =
    let tweaksets =
            let baseNumbers = sortOn (\(a, b) -> (max a b)) (cartesianProduct [0..99] [0..99]) in
            [[(1, x), (2, y)] | (x, y) <- baseNumbers] :: [[(Int, Int)]]
    in do
    program <- getIntsFromConsoleUntilBlank;
    return (findIntCodeTweakWithResult program requestedNumber tweaksets)







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

solveDay7Part2 program =
    let
        modesets = List.permutations [5,6,7,8,9]
        resultsForModeset :: [[Int]] -> Maybe Int
        resultsForModeset modeset =
            let machines = buildSequencerMachines program modeset [0] in
            case last $ sequenceMachinesCyclicallyUntilLastExits machines of
                ((_, o, _), Right True) -> Just $ last o
                (_, Right False) -> Nothing -- Exited with error
                _ -> error "Machine 'exited' without exiting?"
        modesetsToResults :: [([Int], Int)]
        modesetsToResults = map (\(m, Just x) -> (m, x)) $ filter (\case (_, Just _) -> True ; (_, Nothing) -> False) $ map
            (\modeset -> (modeset, resultsForModeset $ map return modeset))
            modesets
        maximizingModesetAndResult = List.maximumBy (comparing snd) modesetsToResults
    in
    snd maximizingModesetAndResult

solveDay7Part2FromConsole = do
    program <- getIntsFromConsoleUntilBlank
    return $ solveDay7Part2 program
