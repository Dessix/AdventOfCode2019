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
module C06 where

import Control.Monad (when, unless)
import qualified Control.Monad.Fail

import Data.Function ((&))
import Data.List
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
-- import Algebra.Graph.Class
import qualified Algebra.Graph.Class as G
import qualified Algebra.Graph.HigherKinded.Class as GHK
import qualified Algebra.Graph.Labelled as LG
import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AMA
import Algebra.Graph.ToGraph (ToGraph (..))
import qualified Algebra.Graph.ToGraph as TG
import Data.Tree
-- import Algebra.Graph.AdjacencyIntMap

import Algebra.Graph.Export.Dot
import Data.GraphViz.Types (parseDotGraphLiberally)
import Data.GraphViz.Types.Canonical (DotGraph)
import Data.GraphViz.Commands

import Text.Printf
import Debug.Trace
import Control.Concurrent (forkIO)
import qualified GHC.Conc.Sync (ThreadId)

import Utils


parseOrbitMapLines :: [T.Text] -> AM.AdjacencyMap T.Text
parseOrbitMapLines mapLines =
    let
        splitItem item =
            let ~(start : end : _) = T.splitOn ")" item in (start, end)
    in
    AM.edges $ map (splitItem) mapLines

parseOrbitMap :: T.Text -> AM.AdjacencyMap T.Text
parseOrbitMap mapStr = parseOrbitMapLines $ T.lines mapStr

--findGraphSinks :: (Graph g, Hashable v, Eq v, Ord v) -> g v e -> 

totalTipDepth :: (Ord t) => t -> AdjacencyMap t -> Int
totalTipDepth root g =
    let
        folder :: [[Int]] -> [Int] -- Count orbits backward from every system, including intermediates
        folder memos = (0::Int) : (concatMap (map succ) memos)
        countTips :: Forest a -> Int
        countTips a = sum $ concatMap (foldTree (\_ a -> folder a)) a
    in
    countTips $ TG.dfsForestFrom [root] g

gr = AM.edges ([("B","C"),("B","G"),("C","D"),("COM","B"),("D","E"),("D","I"),("E","F"),("E","J"),("G","H"),("J","K"),("K","L")] :: [(T.Text, T.Text)])
readGraphFromConsole = Utils.getLinesUntilBlank >>= (return . parseOrbitMapLines . (map T.pack))

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

