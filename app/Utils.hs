module Utils where

import Data.Array.IArray
import Data.Array.MArray

getLinesUntilBlank :: IO [String]
getLinesUntilBlank = do
    a <- getLine
    if a == "" then return [] else do
        b <- getLinesUntilBlank
        return (a : b)

arrayOfList :: [a] -> Array Int a
arrayOfList items = array (0, (length items) - 1) (zip [0..] items)

-- assert :: Bool -> a -> a
-- assert False x = error "Assertion failed!"
-- assert _ x = x

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

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
