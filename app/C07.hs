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

solveDay7Part1 program =
    let
        modesets = List.permutations [0..4]
        resultsForModeset :: [[Int]] -> Maybe Int
        resultsForModeset modeset =
            let machines = buildSequencerMachines program modeset [0] in
            case last $ sequenceMachinesDirectionallyUntilLastExits machines of
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

solveDay7Part1FromConsole = do
    program <- getIntsFromConsoleUntilBlank
    return $ solveDay7Part1 program

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
