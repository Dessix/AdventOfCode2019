module C04 where

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

digitsSmallFirst :: (Integral i) => i -> i -> [i]
digitsSmallFirst base 0 = []
digitsSmallFirst base i =
    let remainder = i `mod` base
        lower = i `div` base in
    remainder : (digitsSmallFirst base lower)

digits :: (Integral i) => i -> i -> [i]
digits base = reverse . digitsSmallFirst base

hasAdjacentDuplicates :: (Eq a) => [a] -> Bool
hasAdjacentDuplicates [] = False
hasAdjacentDuplicates (a : []) = False
hasAdjacentDuplicates (a : b : xs) = a == b || hasAdjacentDuplicates (b : xs)

monotonicallyIncreasing :: (Ord a) => [a] -> Bool
monotonicallyIncreasing [] = True
monotonicallyIncreasing (a : []) = True
monotonicallyIncreasing (a : b : xs) = a <= b && monotonicallyIncreasing (b : xs)

puzzlePartAPredicate a =
    let a' = digits 10 a in
    (monotonicallyIncreasing a') && (hasAdjacentDuplicates a')
solvePuzzlePartA = [136760..595730] & filter puzzlePartAPredicate & length
