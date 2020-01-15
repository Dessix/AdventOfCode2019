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

import Control.Monad (when, unless)

import qualified Control.Monad.Fail

import Data.Function (on)
import Data.List
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Data.Vector.Mutable (MVector, IOVector)
import qualified Data.Vector.Mutable as MVector
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Function ((&))

import Polysemy (runM, raise)
import Polysemy (Sem (..), Member (..))
import Polysemy.Embed (embed, Embed (..))

import Text.Printf
import Debug.Trace

import Control.Monad.ST
import Control.Concurrent
import Control.Concurrent.Chan

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


sequenceMultipleInterpreters :: [Int] -> [Int] -> Int -> [(MemIState, Either String (Maybe Int))]
sequenceMultipleInterpreters program initialInputs 0 = []
sequenceMultipleInterpreters program initialInputs numInterpreters =
    let
        initialState = buildInterpreterInitialState program initialInputs
        -- Run all machines, feeding outputs through to the next until each one reaches a "Right" result
        -- For each machine, (state, processed outputs, Either NextPosition or Result)
        -- Current always gets appended to the list; convert to dequeue when algorithm is stable
        loop :: [Int] -> 
            [(MemIState, [Int], Either Int (Maybe Int))]
            -> [(MemIState, [Int], Either Int (Maybe Int))]
        loop idx [] = undefined
        -- outputs already processed as result is present, pass nothing on, resume next machine
        loop _ ((cur@(_, _, Right _)) : rest) = undefined -- If all existing machines have results, bail out
        -- continue machine, process outputs; if result provided, machine should be registered as "right"
        loop _ ((cur@(state, processedOutputs, Left nextPos)) : rest) = undefined

        initialMachines = List.replicate numInterpreters (initialState)
        idxs = List.cycle [0..(numInterpreters - 1)]
    in
    -- loop idxs
    undefined -- resumeInterpreterInMemory 



-- Proof that IO results can be consumed iteratively
testThreadBlocking = do
    a <- newChan
    b <- newChan
    forkIO $ do
        writeChan a "1"
        threadDelay 20000000
        writeChan a "2"
    forkIO $ do
        contents <- getChanContents a
        writeList2Chan b contents
    
    items <- getChanContents b
    mapM_ (putStrLn . show) items

testThreadBlocking2 = do
    a <- newChan
    b <- newChan
    c <- newChan
    writeChan a 1
    forkIO $ do
        items <- getChanContents a
        let stream = takeWhiler (\x -> x < 1000) $ scanl (\memo item -> item) 1 items 
            in do
            writeList2Chan b stream
            writeChan c $ last $ stream
    forkIO $ do
        contents <- getChanContents b
        writeList2Chan a $ map (* 2) contents
    
    result <- readChan c
    putStrLn $ show result


runInterpreterThreadOnChannels :: (Member (Embed IO) r) => [Int] -> (Chan Int, Chan Int) -> Sem (Interpreter ': r) a -> Sem r (Maybe Int, Either String a)
runInterpreterThreadOnChannels program (inchan, outchan) instructions = do
    program <- embed $ listToMVector program
    inputs <- embed $ getChanContents inchan
    ((_, mem), (outputs, res)) <- runInterpreterIOStreaming (inputs, program) instructions

    embed $ writeList2Chan outchan outputs
    return $ (if outputs /= [] then Just (last outputs) else Nothing, res)


testInterpreterSimultaneity = do
    inchan <- newChan
    outchan <- newChan
    forkIO $ runM $ do
        runInterpreterThreadOnChannels [0,0,0,0] (inchan, outchan) $ do
            v <- input'
            output' (v * 2)
        return ()
    consoleRead <- getLine
    writeChan inchan (read @Int consoleRead)
    result <- readChan outchan
    putStrLn (show result)
