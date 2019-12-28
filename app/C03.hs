module C03 where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.ST

import Data.Array.IArray
import Data.Array.MArray
import qualified Data.Array.ST
import Data.Function
import Data.Functor
import Data.List
import Data.Ratio
import qualified Data.Vector
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

import Text.Printf

import Utils

data Direction = DUp | DDown | DLeft | DRight deriving (Show)

newtype WireSequence = WireSequence [(Direction, Word)]
instance Show WireSequence where
    show (WireSequence seq) = show seq

parseDirectionCode :: String -> (Direction, Word)
parseDirectionCode code =
    let distance = (read (tail code)) in
    let direction = case (head code) of
            'U' -> DUp
            'D' -> DDown
            'L' -> DLeft
            'R' -> DRight
            c -> error $ printf "Invalid direction code %s given" c
    in
    (direction, distance)

parsePuzzleLines :: [String] -> [WireSequence]
parsePuzzleLines plines =
    let entriesMapper l =
            let itemStrings = (T.splitOn (T.pack ",") (T.pack l)) in
            WireSequence $ map parseDirectionCode $ map T.unpack itemStrings
    in
    plines & map entriesMapper

findWireIntersections :: [WireSequence] -> [(Int, Int)]
findWireIntersections =
    undefined

solvePuzzleFromLines :: IO ()
solvePuzzleFromLines =
    do
    l <- Utils.getLinesUntilBlank
    let solution = findWireIntersections $ parsePuzzleLines l in
        putStrLn $ printf "Solved! %s" $ show solution
