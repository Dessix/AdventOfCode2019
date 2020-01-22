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
--{-# LANGUAGE OverloadedStrings #-}
module C08 where

import Control.Monad (when, unless, replicateM, forM_)
import qualified Control.Monad.Parallel as Par

import qualified Control.Monad.Fail

import Data.Char (isDigit, digitToInt, intToDigit)
import Data.Function (on, (&), fix)
import Data.Functor ((<&>))
import Data.Ord (comparing)
import Data.Int (Int8)
import Data.List
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
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
import Control.Monad
import Control.Monad.ST
import Control.Concurrent hiding (Chan, newChan, readChan, writeChan, getChanContents, writeList2Chan, dupChan)
import Control.Concurrent.Chan.Unagi

import Utils

data SIFLayer = SIFLayer { layerSize :: (Word, Word), pixels :: Vector Int8 }
data SIFImage = SIFImage { imageSize :: (Word, Word), layers :: [SIFLayer] }

instance Show SIFLayer where
    show (SIFLayer { layerSize=(xdim, ydim), pixels=pixels }) = printf "<(%d, %d): %s>" xdim ydim (map (intToDigit . fromIntegral) $ Vector.toList pixels)

instance Show SIFImage where
    show (SIFImage { imageSize=(xdim, ydim), layers=layers }) =
        printf "<(%d, %d): %s>" xdim ydim $ concat $ (flip map) layers $ \(SIFLayer { pixels=pixels }) -> map (intToDigit . fromIntegral) $ Vector.toList pixels

tryReadPixelRow :: Word -> String -> Vector Int8 -> Maybe (Vector Int8, String)
tryReadPixelRow 0 remaining pixels = Just (pixels, remaining)
tryReadPixelRow dim [] _ | dim > 0 = Nothing -- Out of data to read, fail
tryReadPixelRow dim (digit:xs) pixels | isDigit digit =
    tryReadPixelRow (dim - 1) xs (Vector.snoc pixels $ fromIntegral $ digitToInt digit)
tryReadPixelRow _ _ _ = Nothing -- Non-digit character

tryReadSifLayer :: (Word, Word) -> String -> Maybe (SIFLayer, String) 
tryReadSifLayer dim@(0, 0) d = Just (SIFLayer { layerSize=dim, pixels = Vector.empty }, d)
tryReadSifLayer dim@(xdim, ydim) d =
    let
        tryReadLayerRows :: Vector Int8 -> Word -> Word -> String -> Maybe (Vector Int8, String)
        tryReadLayerRows pixels _ 0 d = Just (pixels, d)
        tryReadLayerRows pixels xdim ydim d =
            tryReadPixelRow xdim d pixels
                >>= \(pixels', d') -> tryReadLayerRows pixels' xdim (ydim - 1) d'
    in
        tryReadLayerRows Vector.empty xdim ydim d
            <&> \(pixels, remaining) -> (SIFLayer { layerSize = dim, pixels = pixels }, remaining)


tryReadImage :: (Word, Word) -> Text -> Maybe SIFImage
tryReadImage dims digits =
    let
        tryReadImageLayersRev :: [SIFLayer] -> (Word, Word) -> String -> Maybe ([SIFLayer], String)
        tryReadImageLayersRev layers _ [] = Just (layers, [])
        tryReadImageLayersRev layers dims d =
            tryReadSifLayer dims d
                >>= \(layer, remaining) -> tryReadImageLayersRev (layer : layers) dims remaining
    in
        tryReadImageLayersRev [] dims (T.unpack digits)
            <&> \(layers, _) -> SIFImage { imageSize=dims, layers=(reverse layers)}


solveDay8Part1 = do
    raw <- getLine
    case tryReadImage (25, 6) (T.pack raw) of
        Nothing -> return Nothing
        Just img ->
            let
                countOf x items = length $ filter (x==) items
                leastZerosLayer = minimumBy (comparing (\l -> countOf 0 (Vector.toList $ pixels l))) (layers img)
                pixelsInLayer = Vector.toList $ pixels leastZerosLayer
                in
                    return $ Just $ ((countOf 1 pixelsInLayer) * (countOf 2 pixelsInLayer))


data SIFColor = SIFBlack | SIFWhite | SIFTransparent
    deriving (Show, Eq, Ord)

class PixelLookup a where
    getPixel :: a -> (Word, Word) -> SIFColor

instance PixelLookup SIFLayer where
    getPixel SIFLayer { layerSize=(xdim, ydim), pixels=pixels } (x, y) =
        case pixels Vector.! (fromIntegral ((xdim * y) + x)) of
            0 -> SIFBlack
            1 -> SIFWhite
            2 -> SIFTransparent
            _ -> error "Invalid pixel color"

instance PixelLookup SIFImage where
    getPixel SIFImage { layers=layers } pos =
        flip fix layers $ \f layers ->
            case layers of
                [] -> error $ printf "No pixel data at pos %s - transparent pixel in last layer?" (show pos)
                (l : ls) ->
                    case getPixel l pos of
                        SIFTransparent -> f ls
                        c -> c

testImage = Maybe.fromJust $ tryReadImage (2, 2) (T.pack "0222112222120000")

drawImageToText :: SIFImage -> Text
drawImageToText img@(SIFImage { imageSize=(xdim, ydim) }) =
    let
        charForColor :: SIFColor -> Char
        charForColor = \case
            SIFBlack -> 'X'
            SIFWhite -> ' '
            SIFTransparent -> ' '
        textForLine :: Word -> Text
        textForLine y = T.pack $ map (charForColor . (\x -> getPixel img (x, y))) [0..((fromIntegral xdim)-1)]
    in
    T.unlines $ map (\y -> textForLine y) [0..((fromIntegral ydim)-1)]

solveDay8Part2 = do
    raw <- getLine
    case tryReadImage (25, 6) (T.pack raw) of
        Nothing -> return ()
        Just img ->
            do
                putStrLn $ T.unpack $ drawImageToText img
