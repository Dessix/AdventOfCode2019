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

import Text.Printf
import Debug.Trace
import Control.Concurrent (forkIO)
import qualified GHC.Conc.Sync (ThreadId)

import Utils


--findGraphSinks :: (Graph g, Hashable v, Eq v, Ord v) -> g v e -> 


gr = AM.edges ([("B","C"),("B","G"),("C","D"),("COM","B"),("D","E"),("D","I"),("E","F"),("E","J"),("G","H"),("J","K"),("K","L")] :: [(T.Text, T.Text)])

solveSantaPuzzle = do
    orbitMap <- readOrbitMapFromConsole
    case pathBetweenNodes (AM.symmetricClosure orbitMap) (T.pack "YOU") (T.pack "SAN") of
        Just (cost, path) -> return $ cost - 2
        Nothing -> fail "No path available"