{- LANGUAGE FlexibleContext, GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances -}
module Graph
        ( SoGraph(..)
        , makeGraph
        , makeGraphIO
        , makeFgl
        , printGraph
        ) where

import Ldd

import Control.Monad.State (get, put, lift, execStateT, runStateT, StateT)
import Control.Monad.Error
import Control.Monad (when)
import Data.List (union, elemIndex)

import qualified Data.Graph.Inductive.Graph as Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Graphviz (graphviz')


data SoGraph = SoGraph { nodes    :: [FilePath]
                       , edges    :: [(FilePath, FilePath)]
                       , packages :: [(FilePath, Package)] }
	deriving (Eq, Show)


createGraph :: SharedObject m => Int -> ErrorT Message (StateT SoGraph m) ()
createGraph depth = do
    st <- lift get
    let current = nodes st !! depth
        depth' = depth + 1
    deps <- lift $ lift $ getDependencies current
    pkg <- lift $ lift $ getPackage current
    case deps of
        Right list -> do
            let nodes' = union (nodes st) list
                edges' = union (map (\x -> (current,x)) list) (edges st)
                pkgs' = case pkg of
                    Nothing -> packages st
                    Just p  -> (current, p):(packages st)
            lift $ put st {nodes = nodes', edges = edges', packages = pkgs'}
            when (depth' < length nodes') $ createGraph depth'
        Left msg -> throwError msg

makeGraph :: (SharedObject m) => [FilePath] -> m (Either Message SoGraph)
makeGraph [] = return $ Left "No input were given."
makeGraph inputs = do
    result <- runSharedObject inputs
    case result of
        (Left err, _) -> return $ Left err
        (Right _, gr) -> return $ Right gr
  where
    runSharedObject inputs =
         let state = SoGraph {nodes = inputs, edges = [], packages = []}
         in runStateT (runErrorT (createGraph 0)) state
                
makeGraphIO :: [FilePath] -> IO (Either Message SoGraph)
makeGraphIO = makeGraph


type DepGraph = Data.Graph.Inductive.PatriciaTree.Gr String ()

makeFgl :: SoGraph -> DepGraph
makeFgl gr =
    let nodes' = nodes gr
        edges' = edges gr
    in Graph.mkGraph (mkNodes nodes') (mkEdges nodes' edges')
  where
    mkNodes = zip [0..]
    mkEdges nodes =
             map (\(from, to) -> ((index nodes from), (index nodes to), ()))
    index es e = case elemIndex e es of
             Nothing -> error "never get here"
             Just x  -> x

printGraph :: SoGraph -> String
printGraph = graphviz' . makeFgl
