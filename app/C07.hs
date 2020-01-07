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
import Control.Monad.Freer

import qualified Control.Monad.Fail

import Data.Function (on)
import Data.List
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Function ((&))

import Text.Printf
import Debug.Trace

import Utils

import Interpreter
import IntCodeInterpreter


_testInput =
    [
        1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,6,19,23,2,23,6,27,1,5,27,31,
        1,31,9,35,2,10,35,39,1,5,39,43,2,43,10,47,1,47,6,51,2,51,6,55,2,55,13,59,
        2,6,59,63,1,63,5,67,1,6,67,71,2,71,9,75,1,6,75,79,2,13,79,83,1,9,83,87,1,
        87,13,91,2,91,10,95,1,6,95,99,1,99,13,103,1,13,103,107,2,107,10,111,1,9,
        111,115,1,115,10,119,1,5,119,123,1,6,123,127,1,10,127,131,1,2,131,135,1,135,10,0,99,2,14,0,0]

testInterpreter = runInterpreterInMemory _testInput [] $ do { writeMemory' 1 12; writeMemory' 2 2; runInterpreterAtPosition True 0; return 0 }

runDiagnosticsEngine consoleInputs = do
    program <- getIntsFromConsoleUntilBlank;
    case
        runInterpreterInMemory program consoleInputs $ do
            runInterpreterAtPosition True 0
            setExitCode' 0
        of
            (_, Left e) -> error (printf "Diagnostic run failed with error: %s" e)
            ((i, o, _), Right res) -> (printf "Diagnostic run succeeded with result %s and outputs: %s" (show res) (show o))


runAmplifier :: Bool -> [Int] -> Int -> Int -> Maybe Int
runAmplifier debug program mode input =
    case
        runInterpreterInMemory program [mode, input] $ do
            runInterpreterAtPosition True 0
            setExitCode' 0
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
            runInterpreterAtPosition True 0
            resultCode <- readMemory' 0
            setExitCode' resultCode
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
