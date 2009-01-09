module Walker
        ( execApp
        , makeDeps
        , createGraphFromState 
        ) where

import Types
import Ldd

import Control.Monad.State (get, put, liftIO, execStateT, StateT)
import Data.List (union, elemIndex)

import qualified Data.Graph.Inductive.Graph as Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)



data AppState = AppState { nodes :: [SoInfo]
                         , edges :: [(SoInfo, SoInfo)] }

type App = StateT AppState IO


execApp :: [SoInfo] -> App a -> IO (AppState)
execApp ns g =
        let state = AppState {nodes = ns, edges = []}
        in execStateT g state

makeDeps :: Int -> App ()
makeDeps depth = do
    st <- get
    let so@(_, path) = (nodes st) !! depth
    deps <- liftIO $ getDependencies path
    let nodes' = union (nodes st) deps
        edges' = union (map (\x -> (so,x)) deps) (edges st)
        depth' = depth + 1
    put st {nodes = nodes', edges = edges'}
    if (depth' < length nodes')
        then makeDeps depth'
        else return ()


type DepGraph = Data.Graph.Inductive.PatriciaTree.Gr String ()

createGraphFromState :: AppState -> DepGraph
createGraphFromState st =
    let nodes' = nodes st
        edges' = edges st
    in Graph.mkGraph (mkNodes nodes') (mkEdges nodes' edges')
  where
    mkNodes = zip [0..] . map fst
    mkEdges nodes =
             map (\(from, to) -> ((index nodes from), (index nodes to), ()))
    index es e = case elemIndex e es of
             Nothing -> error "never get here"
             Just x  -> x
