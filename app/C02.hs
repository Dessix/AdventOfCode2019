module C02 where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.ST
import Data.Function
import Data.Functor
import Data.List
import Data.Ratio
import Debug.Trace
import Text.Printf

import Utils
import C01

readArrayEach :: (MArray a e m, Ix i) => a i e -> [i] -> m [e]
readArrayEach array positions = mapM (readArray array) positions

readArraySequence :: (MArray a e m, Ix i, Num i, Enum i) => a i e -> i -> i -> m [e]
readArraySequence array start count = readArrayEach array [start..start + count - 1]

readArrayEachInBounds :: (MArray a e m, Ix i, Num i, Enum i) => a i e -> [i] -> m (Maybe [e])
readArrayEachInBounds array positions = do
    bounds <- getBounds array
    let boundsCheck = inRange bounds in
        let allIn = (all (==True) (map boundsCheck positions)) :: Bool in
            if allIn then ((do
                results <- (mapM (readArray array) positions)
                return (Just results)
                )) else ((return Nothing))

readArrayPointer :: (MArray a e m, Ix e) => a e e -> e -> m e
readArrayPointer array pointerPos = readArray array pointerPos >>= readArray array

runIntCodeAtPositionST :: (STArray s Int Int) -> Int -> ST s (Maybe (STArray s Int Int))
runIntCodeAtPositionST input position = do 
    opcode <- readArray input position
    case opcode of
        1 -> -- from x, y, write sum to z
            do
            [xp, yp, zp] <- readArraySequence input (position + 1) 3
            destVals <- readArrayEachInBounds input [xp, yp]
            case destVals of
                Nothing -> return Nothing --error "Boundscheck caught"
                Just [x', y'] -> do
                    let z' = x' + y' in do
                        writeArray input zp z'
                        traceM $ printf "Add @%d @%d to @%d values %d + %d = %d" xp yp zp x' y' z'
                    runIntCodeAtPositionST input (position + 4)
        2 -> -- from x, y, write product to z
            do
            [xp, yp, zp] <- readArraySequence input (position + 1) 3
            destVals <- readArrayEachInBounds input [xp, yp]
            case destVals of
                Nothing -> return Nothing --error "Boundscheck caught"
                Just [x', y'] -> do
                    let z' = x' * y' in do
                        writeArray input zp z'
                        traceM $ printf "Mul @%d @%d to @%d values %d + %d = %d" xp yp zp x' y' z'
                    runIntCodeAtPositionST input (position + 4)
        99 -> -- Bail out
            do
            traceM $ printf "Bailing at exit instruction"
            return (Just input)
        unknown -> -- Unknown opcode
            error (printf "Opcode %d is not implemented" unknown)


runIntCodeST :: (STArray s Int Int) -> ST s (Maybe (STArray s Int Int))
runIntCodeST input = runIntCodeAtPositionST input 0

runIntCode :: [Int] -> Maybe [Int]
runIntCode input =
    let inputAsArray = arrayOfList input in
    let results = runST (do
        arr <- thaw inputAsArray
        res <- runIntCodeST arr
        case res of
            Just a -> do
                frozen <- freeze a
                return (Just frozen) 
            Nothing -> return Nothing
        ) in
    liftM (\r -> (Data.Array.IArray.elems (r :: Array Int Int))) results


testIntCode :: [Int] -> [Int] -> String
testIntCode input expected =
    let result = runIntCode input in
    assert (result == (Just expected)) (printf "%s resulted in %s" (show input) (show expected))

runtests =
    let results = (
            (testIntCode [1,0,0,0,99] [2,0,0,0,99]) -- 1 + 1 = 2
            : (testIntCode [2,3,0,3,99] [2,3,0,6,99]) -- 3 * 2 = 6
            : (testIntCode [2,4,4,5,99,0] [2,4,4,5,99,9801]) -- 99 * 99 = 9801
            : (testIntCode [1,1,1,4,99,5,6,0,99] [30,1,1,4,2,5,6,0,99])
            : []
            ) in
    (map (\result -> trace result ()) results)

findIntCodeTweakWithResult :: [Int] -> Int -> [[(Int, Int)]] -> Maybe [(Int, Int)]
findIntCodeTweakWithResult input = findIntCodeTweakWithResultArray (arrayOfList input)

findIntCodeTweakWithResultArray :: Array Int Int -> Int -> [[(Int, Int)]] -> Maybe [(Int, Int)]
findIntCodeTweakWithResultArray _ _ [] = Nothing
findIntCodeTweakWithResultArray input desired (tweaks : rest) =
    let output = runST (do
        arr <- thaw input
        mapM_ (\(pos, newValue) -> writeArray arr pos newValue) tweaks
        runIntCodeST arr
        resultCode <- readArray arr 0
        if resultCode == desired then return (Just tweaks) else do
            traceM (printf "ResultCode incorrect with value %d" resultCode)
            return Nothing
        ) in
    case output of
        Just tweaks -> Just tweaks
        Nothing -> findIntCodeTweakWithResultArray input desired rest


test1202 =
    findIntCodeTweakWithResult [2,13,17,0, 1,0,21,0, 99,0,0,0, 2,100,0,14, 2,0,0,18, 2,0,0,22] 1202 [[(1,400)], [(17,12),(21,2)]]

tweaksets =
    let baseNumbers = sortOn (\(a, b) -> (max a b)) (cartesianProduct [0..99] [0..99]) in
    [[(1, x), (2, y)] | (x, y) <- baseNumbers] :: [[(Int, Int)]]

solvePart2 requestedNumber inputs =
    findIntCodeTweakWithResult inputs requestedNumber tweaksets
