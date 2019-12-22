module C02 where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.ST
import Data.Function
import Data.Functor
import Data.Ratio
import Debug.Trace
import Text.Printf

import Utils
import C01

readArrayEach :: (MArray a e m, Ix i) => a i e -> [i] -> m [e]
readArrayEach array positions = mapM (readArray array) positions

readArraySequence :: (MArray a e m, Ix i, Num i, Enum i) => a i e -> i -> i -> m [e]
readArraySequence array start count = readArrayEach array [start..start + count - 1]

readArrayPointer :: (MArray a e m, Ix e) => a e e -> e -> m e
readArrayPointer array pointerPos = readArray array pointerPos >>= readArray array

runIntCodeAtPositionST :: (STArray s Int Int) -> Int -> ST s (STArray s Int Int)
runIntCodeAtPositionST input position = do 
    opcode <- readArray input position
    case opcode of
        1 -> -- from x, y, write sum to z
            do
            [xp, yp, zp] <- readArraySequence input (position + 1) 3
            [x', y'] <- readArrayEach input [xp, yp]
            let z' = x' + y' in do
                writeArray input zp z'
                traceM $ printf "Add @%d @%d to @%d values %d + %d = %d" xp yp zp x' y' z'
            runIntCodeAtPositionST input (position + 4)
        2 -> -- from x, y, write product to z
            do
            [xp, yp, zp] <- readArraySequence input (position + 1) 3
            [x', y'] <- readArrayEach input [xp, yp]
            let z' = x' * y' in do
                writeArray input zp z'
                traceM $ printf "Mul @%d @%d to @%d values %d + %d = %d" xp yp zp x' y' z'
            runIntCodeAtPositionST input (position + 4)
        99 -> -- Bail out
            do
            traceM $ printf "Bailing at exit instruction"
            return input
        unknown -> -- Unknown opcode
            error (printf "Opcode %d is not implemented" unknown)


runIntCodeST :: (STArray s Int Int) -> ST s (STArray s Int Int)
runIntCodeST input = runIntCodeAtPositionST input 0

runIntCode :: [Int] -> [Int]
runIntCode input =
    let inputAsArray = arrayOfList input in
    let results = runSTArray (thaw inputAsArray >>= runIntCodeST) in
    (Data.Array.IArray.elems (results :: Array Int Int))


testIntCode :: [Int] -> [Int] -> String
testIntCode input expected =
    let result = runIntCode input in
    assert (result == expected) (printf "%s resulted in %s" (show input) (show expected))

runtests =
    let results = (
            (testIntCode [1,0,0,0,99] [2,0,0,0,99]) -- 1 + 1 = 2
            : (testIntCode [2,3,0,3,99] [2,3,0,6,99]) -- 3 * 2 = 6
            : (testIntCode [2,4,4,5,99,0] [2,4,4,5,99,9801]) -- 99 * 99 = 9801
            : (testIntCode [1,1,1,4,99,5,6,0,99] [30,1,1,4,2,5,6,0,99])
            : []
            ) in
    (map (\result -> trace result ()) results)
