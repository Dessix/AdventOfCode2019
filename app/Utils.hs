module Utils where

import Data.Array.IArray

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
