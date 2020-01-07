{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Utils where

import Data.Array.IArray
import Data.Array.MArray
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as T.Read
import Data.Maybe

import qualified Algebra.Graph.Class as G
import qualified Algebra.Graph.HigherKinded.Class as GHK
import qualified Algebra.Graph.Labelled as LG
import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AMA
import Algebra.Graph.ToGraph (ToGraph (..))
import qualified Algebra.Graph.ToGraph as TG
import Algorithm.Search as Search
import Data.Tree

import Algebra.Graph.Export.Dot
import Data.GraphViz.Types (parseDotGraphLiberally)
import Data.GraphViz.Types.Canonical (DotGraph)
import Data.GraphViz.Commands

import Control.Concurrent (forkIO)
import qualified GHC.Conc.Sync (ThreadId)
import FreerUtils

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

parseIntsFromStringLine :: String -> [Int]
parseIntsFromStringLine l =
    let
        l' = T.pack l
    in
    concatMap
        ((\s ->
            maybeToList $
            if (T.length s) > 0 then (case ((T.Read.signed T.Read.decimal) s) of Right (x, _) -> Just x ; _ -> Nothing) else Nothing) . T.strip)
        (T.splitOn (T.pack ",") l')

getIntsFromConsoleUntilBlank :: IO [Int]
getIntsFromConsoleUntilBlank = do
    results <- (fmap (parseIntsFromStringLine)) <$> Utils.getLinesUntilBlank
    return $ concat results

digitsRightToLeftUnfolderR :: Int -> Maybe (Int, Int)
digitsRightToLeftUnfolderR memo =
    if memo > 0 then
        Just $ let modded = memo `mod` 10 in (modded, (memo - modded) `div` 10)
    else Nothing

digitsRightToLeft :: Int -> [Int]
digitsRightToLeft = List.unfoldr digitsRightToLeftUnfolderR

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : xs) = Just x

headsMatch :: (Eq t) => [[t]] -> Bool
headsMatch items = allEq $ map safeHead items

-- allEq :: Eq a => [a] -> Bool
-- allEq [] = True
-- allEq (x : []) = True
-- allEq (x : y : xs) = if x == y then allEq (y : xs) else False

asOverlappingPairs :: [t] -> [(t, t)]
asOverlappingPairs = zip <*> tail

allEq :: Eq a => [a] -> Bool
allEq = (all (uncurry (==))) . asOverlappingPairs

foldlMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldlMaybe f a bs =
   foldr (\b g x -> f x b >>= g) Just bs a

-- Graph utilities

parseOrbitMapLines :: [T.Text] -> AM.AdjacencyMap T.Text
parseOrbitMapLines mapLines =
    let
        splitItem item =
            let ~(start : end : _) = T.splitOn ")" item in (start, end)
    in
    AM.edges $ map (splitItem) mapLines

parseOrbitMap :: T.Text -> AM.AdjacencyMap T.Text
parseOrbitMap mapStr = parseOrbitMapLines $ T.lines mapStr

readOrbitMapFromConsole = Utils.getLinesUntilBlank >>= (return . parseOrbitMapLines . (map T.pack))

graphStyle :: Style T.Text String
graphStyle =
    Style {
        graphName               = "Example"
        , preamble                = [""]
        , graphAttributes         = ["label" := "Example", "labelloc" := "top"]
        , defaultVertexAttributes = ["shape" := "circle"]
        , defaultEdgeAttributes   = mempty
        , vertexName              = \x   -> (T.unpack x)
        , vertexAttributes        = \x   -> ["color" := "blue"   ]
        , edgeAttributes          = \x y -> ["style" := "dashed" ] }

drawGraph :: (ToGraph g, ToVertex g ~ T.Text) => g -> IO GHC.Conc.Sync.ThreadId
drawGraph g = forkIO $ runGraphvizCanvas' ((parseDotGraphLiberally $ TL.pack $ export graphStyle g) :: DotGraph String) Xlib

totalTipDepth :: (Ord t) => t -> AdjacencyMap t -> Int
totalTipDepth root g =
    let
        folder :: [[Int]] -> [Int] -- Count orbits backward from every system, including intermediates
        folder memos = (0::Int) : (concatMap (map succ) memos)
        countTips :: Forest a -> Int
        countTips a = sum $ concatMap (foldTree (\_ a -> folder a)) a
    in
    countTips $ TG.dfsForestFrom [root] g

findAncestorsOfNode :: (ToGraph t, Ord (ToVertex t)) => t -> (ToVertex t) -> [ToVertex t]
findAncestorsOfNode transposedGraph node = TG.dfs [node] transposedGraph

findLimitedTransposedDagCommonAncestor :: (Eq v, Ord v) => AdjacencyMap v -> [v] -> Maybe v
findLimitedTransposedDagCommonAncestor _ [] = Nothing
findLimitedTransposedDagCommonAncestor g children =
    let
        rootTrees = map (reverse . findAncestorsOfNode g) children
        matching = takeWhile (\x -> (allEq x) && x /= [] && (head x) /= Nothing) $ List.transpose $ map (\l -> List.concat [(map Just l), (List.repeat Nothing)]) rootTrees
    in
    if matching == [] then Nothing
    else head $ last matching

pathBetweenNodes :: (Eq v, Ord v) => AdjacencyMap v -> v -> v -> Maybe (Int, [v])
pathBetweenNodes g start dest =
    Search.dijkstra
    (\vert -> AM.postSet vert g)
    (\_ _ -> 1)
    (\vert -> vert == dest)
    start
