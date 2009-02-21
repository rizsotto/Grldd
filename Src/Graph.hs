{- LANGUAGE ScopedTypeVariables -}
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

import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Graphviz (graphviz')

import qualified Data.Map as M

---------- generic part of collecting dependency informations
type Moves a b = ([a], b)
type Map a b = M.Map a ([a], b)

bfsM :: (Monad m, Ord a) =>
        (a -> m (Moves a b)) -> Map a b -> [a] -> m (Map a b)
bfsM f m []     = return m
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

inspect :: [FilePath] -> IO (DepInfo)
inspect fns = bfsM getInfo M.empty fns


---------- graph printing part (typed on FilePath, Maybe Package)
type DepGraph = Data.Graph.Inductive.PatriciaTree.Gr String ()

mkGraph' :: DepInfo -> DepGraph
mkGraph' deps =
    if M.null deps
        then Graph.empty 
        else Graph.mkGraph nodes edges
  where
    nodes = M.foldWithKey (\k _ acc -> ((M.findIndex k deps),k):acc) [] deps
    edges = M.foldWithKey (\k (v,_) acc ->
                (map (\dep -> ((M.findIndex k deps),
                               (M.findIndex dep deps),
                               ())) v) ++ acc) [] deps

print' :: DepInfo -> IO ()
print' = putStrLn . graphviz' . mkGraph'
