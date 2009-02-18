module Main where

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

reportResult :: Either Message SoGraph -> IO ()
reportResult (Left e) = putStrLn ("grldd failed with error: " ++ show e)
reportResult (Right g) = putStrLn $ graphviz' $ makeFgl g

main = do
    files <- parseArgs
    r <- makeGraphIO files
    reportResult r
  where
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