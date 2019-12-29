module C04 where

import Control.Arrow
import Control.Exception
import Control.Lens
import Control.Monad
import qualified Control.Monad.Fail
import Control.Monad.ST

import Data.Array.IArray
import Data.Array.MArray
import qualified Data.Array.ST
import Data.Array.ST (STUArray)
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


runIntCodeAtPositionST :: (MArray a e m, Ix e, Num e, Enum e, PrintfArg e, Num e, Integral e, Control.Monad.Fail.MonadFail m) => (a e e) -> e -> m (Maybe (a e e))
runIntCodeAtPositionST input position =
    let positionString = (printf "% 8d |" position) :: String in
    do
    opcode <- readArray input position
    case opcode of
        1 -> -- from x, y, write sum to z
            do
            [xp, yp, zp] <- readArraySequence input (succ position) 3
            destVals <- readArrayEachInBounds input [xp, yp]
            case destVals of
                Nothing -> return Nothing --error "Boundscheck caught"
                Just [x', y'] -> do
                    let z' = x' + y' in do
                        writeArray input zp z'
                        traceM $ printf "%s add %10d @ %-5d %10d @ %-5d -> %10d @ %-5d" positionString x' xp y' yp z' zp
                    runIntCodeAtPositionST input (position + 4)
        2 -> -- from x, y, write product to z
            do
            [xp, yp, zp] <- readArraySequence input (succ position) 3
            destVals <- readArrayEachInBounds input [xp, yp]
            case destVals of
                Nothing -> return Nothing --error "Boundscheck caught"
                Just [x', y'] ->
                    let z' = x' * y' in do
                        writeArray input zp z'
                        traceM $ printf "%s mul %10d @ %-5d %10d @ %-5d -> %10d @ %-5d" positionString x' xp y' yp z' zp
                        runIntCodeAtPositionST input (position + 4)
        3 -> -- from x, write to y
            do
            [xp, yp] <- readArraySequence input (succ position) 3
            params <- readArrayEachInBounds input [xp]
            case params of
                Nothing -> return Nothing --error "Boundscheck caught"
                Just [x'] -> do
                    writeArray input yp x'
                    traceM $ printf "%s set %10d @ %-5d -> %10d @ %-5d" positionString x' xp x' yp
                    runIntCodeAtPositionST input (position + 3)
        99 -> -- Bail out
            do
            traceM $ printf "%s hcf" positionString
            return (Just input)
        unknown -> -- Unknown opcode
            error (printf "Opcode %d is not implemented" unknown)


runIntCodeST :: (MArray a e m, Ix e, Num e, Enum e, PrintfArg e, Integral e, Control.Monad.Fail.MonadFail m) => (a e e) -> m (Maybe (a e e))
runIntCodeST input = runIntCodeAtPositionST input 0

runIntCode :: [Int] -> Maybe [Int]
runIntCode input =
    let inputAsArray = (arrayOfList input :: Array Int Int) in
    let results = (runST (do
        arr <- (Data.Array.ST.thaw inputAsArray :: ST s (STUArray s Int Int))
        res <- runIntCodeST arr
        mapM freeze res
        )) :: Maybe (Array Int Int) in
    liftM Data.Array.IArray.elems results
