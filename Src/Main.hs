module Main where

import Types
import Ldd
import Graph

import Control.Exception
import Data.Graph.Inductive.Graphviz (graphviz')
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Console.GetOpt


data Flag = Help
  deriving Eq

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Show this help message" ]



main = do
    files <- parseArgs
    handle handler
           (do s <- execApp (mkInfo files) (makeDeps 0)
               putStrLn $ graphviz' $ createGraphFromState s )
  where
    mkInfo :: [String] -> [SoInfo]
    mkInfo = map (\x -> (parsePath x, x))

    handler :: SomeException -> IO ()
    handler e = dump (show e) >> exitWith (ExitFailure 1)

    parseArgs :: IO ([String])
    parseArgs = do
            argv <- getArgs
            case parse argv of
                        ([], [], [])           -> help
                        ([], files, [])        -> return files
                        (opts, files, [])      -> help
                        (_,_,errs)             -> die errs

    parse      = getOpt Permute options
    header     = "Usage: grldd [-h] [file ...]"
    info       = usageInfo header options
    dump       = hPutStrLn stderr
    die errs   = dump (concat errs ++ info) >> exitWith (ExitFailure 1)
    help       = dump info                  >> exitWith ExitSuccess
