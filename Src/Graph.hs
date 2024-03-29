{-  Grldd is a dependency tracking tool.
    Copyright (C) 2009-2013  Laszlo Nagy

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE ScopedTypeVariables #-}
module Graph
        ( Moves
        , Map
        , bfsM
        , inspect
        , mkGraph'
        , print'
        ) where

import Ldd
import Dpkg

import qualified Data.Graph.Inductive.Graph as Graph (empty, mkGraph)
import qualified Data.Graph.Inductive.PatriciaTree as Graph (Gr)
import qualified Data.Graph.Inductive.Graphviz as Graph (graphviz')

import qualified Data.Map as M

---------- generic part of collecting dependency informations
type Moves a b = ([a], b)
type Map a b = M.Map a ([a], b)

bfsM :: (Monad m, Ord a) =>
        (a -> m (Moves a b)) -> Map a b -> [a] -> m (Map a b)
bfsM _ m []     = return m
bfsM f m (x:xs) = do
    v@(ys,_) <- f x
    bfsM f (M.insert x v m) (xs ++ filter (`M.notMember` m) ys)


---------- io monad implementation
getInfo :: FilePath -> IO ([FilePath], Maybe Package)
getInfo fn = do
        deps <- getDependencies fn
        pkg <- getPackage fn
        return (deps, pkg)

type DepInfo = Map FilePath (Maybe Package)

inspect :: [FilePath] -> IO DepInfo
inspect = bfsM getInfo M.empty


---------- graph printing part
mkGraph' :: Ord a => Map a b -> Graph.Gr a ()
mkGraph' deps =
    if M.null deps
        then Graph.empty 
        else Graph.mkGraph nodes edges
  where
    nodes = M.foldrWithKey (\k _ acc -> (M.findIndex k deps, k):acc) [] deps
    edges = M.foldrWithKey (\k (v,_) acc ->
                map (\dep -> (M.findIndex k deps,
                              M.findIndex dep deps,
                              ())) v ++ acc) [] deps

print' :: DepInfo -> IO ()
print' = putStrLn . Graph.graphviz' . mkGraph'
