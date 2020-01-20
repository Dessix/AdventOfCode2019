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
module C07 where

import Control.Monad (when, unless, replicateM, forM_)
import qualified Control.Monad.Parallel as Par

import qualified Control.Monad.Fail

import Data.Function (on)
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
import Data.Function ((&))

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
runSingleMachine :: Maybe String -> (MemIState, Either Int ()) -> (MemIState, Either Int ())
runSingleMachine debugName cur@(_, Right _) = cur
runSingleMachine debugName cur@(state, Left nextPos) =
    case resumeInterpreterInMemory state $ do runInterpreterAtPositionYieldingWithDebugName debugName nextPos of
        (state', Right (Just nextPos')) -> (state', Left nextPos')
        (state', Right Nothing) -> (state', Right ())
        (_, Left _) -> case cur of -- An error occurred; handle it
            (((_ : _), _, _), _) -> (state, Right ()) -- Had input available; was not a lack-of-input failure; bail
            _ -> cur -- No input available; may have been lack-of-input, continue next cycle

-- Runs a machine, producing a new state and outputs to feed to the next
runMachineStep :: Maybe String -> (MemIState, Either Int ()) -> ((MemIState, Either Int ()), [Int])
runMachineStep debugName cur@(_, Right _) = (cur, [])
runMachineStep debugName ((i, o, state), Left nextPos) =
    let ((i', o', state'), result) = runSingleMachine debugName ((i, [], state), Left nextPos)
    in (((i', o ++ o', state'), result), o')


foldMachineSet ::
    [(Int, (MemIState, Either Int ()))]
    -> [(Int, (MemIState, [Int], Either Int ()))]
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
    [(MemIState, Either Int ())]
    -> [(MemIState, [Int], Either Int ())]
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
buildSequencerMachines :: [Int] -> [[Int]] -> [Int] -> [(MemIState, Either Int ())]
buildSequencerMachines program initialInputs starterInputs =
    map (\x->(x, Left 0)) $ buildSequencerInitialStates program initialInputs starterInputs


runMachineSequencerUntil ::
    ([(MemIState, [Int], Either Int ())] -> [(MemIState, [Int], Either Int ())])
    -> ([(MemIState, [Int], Either Int ())] -> Bool)
    -> [(MemIState, Either Int ())]
    -> [(MemIState, Either Int ())]
runMachineSequencerUntil sequencer predicate [] = []
runMachineSequencerUntil sequencer predicate machines =
    let
        res = runMachineSet machines
        res' = map (\(s, o, r) -> (s, r)) $ sequencer res
    in if predicate res then res' else runMachineSequencerUntil sequencer predicate res'

sequenceMachinesDirectionallyUntil :: ([(MemIState, [Int], Either Int ())] -> Bool) -> [(MemIState, Either Int ())] -> [(MemIState, Either Int ())]
sequenceMachinesDirectionallyUntil predicate machines = runMachineSequencerUntil id predicate machines

sequenceMachinesCyclicallyUntil :: ([(MemIState, [Int], Either Int ())] -> Bool) -> [(MemIState, Either Int ())] -> [(MemIState, Either Int ())]
sequenceMachinesCyclicallyUntil predicate machines =
    let
        selectNewOutputs (_, o', _) = o'
        addNewInputs ((i, o, s), o', r) i' = ((i ++ i', o, s), o', r)
        sequencer :: [(MemIState, [Int], Either Int ())] -> [(MemIState, [Int], Either Int ())]
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

-- Proof that IO results can be consumed iteratively
-- testThreadBlocking = do
--     a <- newChan
--     b <- newChan
--     forkIO $ do
--         writeChan a "1"
--         threadDelay 20000000
--         writeChan a "2"
--     forkIO $ do
--         contents <- getChanContents a
--         writeList2Chan b contents
    
--     items <- getChanContents b
--     mapM_ (putStrLn . show) items

-- testThreadBlocking2 = do
--     a <- newChan
--     b <- newChan
--     c <- newChan
--     writeChan a 1
--     forkIO $ do
--         items <- getChanContents a
--         let stream = takeWhiler (\x -> x < 1000) $ scanl (\memo item -> item) 1 items 
--             in do
--             writeList2Chan b stream
--             writeChan c $ last $ stream
--     forkIO $ do
--         contents <- getChanContents b
--         writeList2Chan a $ map (* 2) contents
    
--     result <- readChan c
--     putStrLn $ show result


runInterpreterThreadOnChannels :: (Member (Embed IO) r) => [Int] -> [Int] -> (OutChan Int, InChan Int) -> Sem (Interpreter ': r) a -> Sem r (Maybe Int, Either String a)
runInterpreterThreadOnChannels program baseInputs (inchan, outchan) instructions = do
    program <- embed $ listToMVector program
    inputs <- embed $ getChanContents inchan
    ((_, mem), (outputs, res)) <- runInterpreterIOStreaming ((baseInputs ++ inputs), program) instructions

    embed $ writeList2Chan outchan outputs
    return $ (if outputs /= [] then Just (last outputs) else Nothing, res)


-- testInterpreterSimultaneity = do
--     inchan <- newChan
--     outchan <- newChan
--     forkIO $ runM $ do
--         runInterpreterThreadOnChannels [0,0,0,0] [] (inchan, outchan) $ do
--             v <- input'
--             output' (v * 2)
--         return ()
--     consoleRead <- getLine
--     writeChan inchan (read @Int consoleRead)
--     result <- readChan outchan
--     putStrLn (show result)

nameObjects nameSequence objects =
    Map.fromList (zip (Set.toList (Set.fromList objects)) nameSequence)

sequenceMultipleInterpretersIONoLoop :: [Int] -> [[Int]] -> Int -> IO (Either String Int)
sequenceMultipleInterpretersIONoLoop program blockInitialInputs initializerInput =
    let
        section :: (Member (Embed IO) r) => (OutChan Int, InChan Int) -> [Int] -> Sem (Interpreter ': r) () -> Sem r ()
        section (inchan, outchan) initialInputs instructions = do
            runInterpreterThreadOnChannels program initialInputs (inchan, outchan) instructions
            return ()
        blockCount = List.length blockInitialInputs
    in do
    traceM $ printf "Blockcount is %d" blockCount
    channels <- replicateM (blockCount + 1) newChan
    let
        blockChanSets =
            map (\((_, io), (oi, _)) -> (io, oi)) $ take blockCount $ slidingPairs (channels)
        makeSection (id, (iocs, initialInputs)) =
            section iocs initialInputs $ do
                traceM $ printf "I#%d has initial inputs %s" id (show initialInputs)
                let
                    loop pos = do
                        res <- runInterpreterAtPositionYieldingWithDebugName (Just ("I#"++(show id))) pos
                        --traceM $ printf "I#%d yielding with nextPos of %s" id (show res)
                        --embed $ yield
                        case res of
                            Just next -> loop next
                            Nothing -> do
                                traceM $ printf "Exiting interpreter %d" id
                                return ()
                    in
                    loop 0
        sections :: [Sem '[Embed IO] ()]
        sections = map makeSection
            (zip
                ([0..]::[Int])
                (zip
                    blockChanSets
                    blockInitialInputs
               ))
        globalInputChannel = head channels
        in do
        traceM "Writing initial input"
        writeChan (fst globalInputChannel) initializerInput
        traceM $ printf "Starting threads for %d sections" (List.length sections)
        Par.mapM (Polysemy.runM) sections
        traceM "Gathering results..."
        results <- getChanContents (snd (last channels))
        forM_ results (\r -> putStrLn (show r))
        return $ Right $ last results

testSequenceMultipleInterpretersNoLoop =
    sequenceMultipleInterpretersIONoLoop
        [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
        --(map (\x->[x]) [4,3,2,1,0])
        (map (\x->[x]) [4])
        0

sequenceMultipleInterpretersIO :: [Int] -> [[Int]] -> Int -> IO (Either String Int)
sequenceMultipleInterpretersIO program blockInitialInputs initializerInput =
    let
        section :: (Member (Embed IO) r) => (OutChan Int, InChan Int) -> [Int] -> Sem (Interpreter ': r) () -> Sem r ()
        section (inchan, outchan) initialInputs instructions = do
            runInterpreterThreadOnChannels program initialInputs (inchan, outchan) instructions
            return ()
        blockCount = List.length blockInitialInputs
    in do
    channels <- replicateM blockCount newChan
    -- First of `channels` is global input and also global output; arrange pairings to fit
    -- a,b,c; cycle, pairize => [ab, bc, ca] leads to [... | a 1 b 2 c 3 | a 1 b 2 ...]
    let
        blockChanSets =
            map (\((_, io), (oi, _)) -> (io, oi)) $ take blockCount $ slidingPairs (cycle channels)
        sections :: [Sem '[Embed IO] ()]
        sections = map (\(id, (iocs, initialInputs)) ->
            section iocs initialInputs $ do
                traceM $ printf "I#%d has initial inputs %s" id (show initialInputs)
                let
                    loop pos = do
                        traceM $ printf "I#%d:" id
                        res <- runInterpreterAtPositionYielding True pos
                        traceM $ printf "I#%d yielding with nextPos of %s" id (show res)
                        embed $ yield
                        case res of
                            Just next -> loop next
                            Nothing -> do
                                traceM $ printf "Exiting interpreter %d" id
                                return ()
                    in
                    loop 0
            ) (zip ([0..]::[Int]) (zip blockChanSets blockInitialInputs))
        globalInputChannel = head channels
        in do
        traceM $ "Interpreter connections (i,o): " ++ (show (take blockCount $ slidingPairs (cycle [0..blockCount - 1])))
        traceM "Writing initial input"
        writeChan (fst globalInputChannel) initializerInput
        outputChannel <- dupChan (fst globalInputChannel)
        traceM "Starting threads"
        mapM_ (forkIO . Polysemy.runM) sections
        traceM "Gathering results..."
        results <- getChanContents outputChannel
        forM_ results (\r -> putStrLn (show r))
        return $ Right $ last results


testSequenceMultipleInterpreters =
    sequenceMultipleInterpretersIO
        [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] [[9],[8],[7],[6],[5]]
        0