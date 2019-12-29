module C03 where

import Control.Arrow
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
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple

import Debug.Trace

import Text.Printf

import Utils

data Direction = DUp | DDown | DLeft | DRight
instance Show Direction where
    show d = case d of
            DUp -> "Up"
            DDown -> "Down"
            DLeft -> "Left"
            DRight -> "Right"

data WireNode = WireNode (Direction, Word)
instance Show WireNode where
    show (WireNode (direction, distance)) = printf "%c%d" (head (show direction)) distance

newtype WireSequence = WireSequence [WireNode]
instance Show WireSequence where
    show (WireSequence nodes) = show nodes

parseDirectionCode :: String -> WireNode
parseDirectionCode code =
    let distance = (read (tail code))
        direction = case (head code) of
            'U' -> DUp
            'D' -> DDown
            'L' -> DLeft
            'R' -> DRight
            c -> error $ printf "Invalid direction code %s given" c
    in
    WireNode (direction, distance)

parsePuzzleLines :: [String] -> [WireSequence]
parsePuzzleLines plines =
    let
        entriesMapper l =
            let itemStrings = (T.splitOn (T.pack ",") (T.pack l)) in
            WireSequence $ map parseDirectionCode $ map T.unpack itemStrings
    in
    plines & map entriesMapper

wirePositions :: (Int, Int) -> WireSequence -> [(Int, Int)]
wirePositions _ (WireSequence []) = []
wirePositions start (WireSequence (WireNode (dir, len) : remaining)) =
    let
        directionalOffset dir = case dir of
            DUp -> (0, -1)
            DDown -> (0, 1)
            DLeft -> (-1, 0)
            DRight -> (1, 0)
        genPositionsForDirection _ _ 0 = []
        genPositionsForDirection (startX, startY) dir len =
            let
                (dx, dy) = directionalOffset dir
                pos' = (startX + dx, startY + dy) in
            pos' : genPositionsForDirection pos' dir (pred len)
        nextPositions = genPositionsForDirection start dir len
    in
    nextPositions ++ (wirePositions (last (start : nextPositions)) (WireSequence remaining))

mapFirstPerKey :: (Ord k) => [(k, v)] -> Map k v
mapFirstPerKey [] = Map.empty
mapFirstPerKey pairs =
    let
        folder :: (Ord k) => Map k v -> (k, v) -> Map k v
        folder memo (k, v) =
            Map.insertWith (curry snd) k v memo
    in
    foldl' folder Map.empty pairs

findWireIntersections :: [WireSequence] -> [((Int, Int), Word)]
findWireIntersections sequences =
    let
        origin = (0, 0)
        wirePositions' nodes = zip (wirePositions origin nodes) [1..]
        buildPositionMap =
            wirePositions' >>> mapFirstPerKey

        posMaps = map buildPositionMap sequences
        intersectionFolder a b =
            Map.intersectionWith (+) a b

    in
    Map.toList $ foldr1 intersectionFolder posMaps

manhattanDistance :: (Int, Int) -> (Int, Int) -> Word
manhattanDistance (x, y) (x', y') = fromIntegral ((abs (x' - x)) + (abs (y' - y)))

solvePuzzleFromLines :: IO ()
solvePuzzleFromLines = do
    l <- Utils.getLinesUntilBlank
    let wireIntersections = findWireIntersections $ parsePuzzleLines l in
        putStrLn $ printf "Solved! %s" $ show $ minimumBy (compare `on` snd) wireIntersections
