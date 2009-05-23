module Main where

import Graph

import Control.Exception
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Console.GetOpt


data Flag = Help
  deriving Eq

options :: [OptDescr Flag]
options = [Option ['h'] ["help"] (NoArg Help) "Show this help message"]


main :: IO ()
main = do
    handle
        (\e -> do 
                dump ("grldd: failed " ++ show e)
                exitWith (ExitFailure 1))
        (parseArgs >>= inspect >>= print' >> exitWith ExitSuccess)
  where
    parseArgs :: IO ([String])
    parseArgs = do
            argv <- getArgs
            case parse argv of
                        ([], [], [])     -> help
                        ([], files, [])  -> return files
                        (_,_,errs)       -> die errs

    parse      = getOpt Permute options
    header     = "Usage: grldd [-h] [file ...]"
    info       = usageInfo header options
    die errs   = dump (concat errs ++ info) >> exitWith (ExitFailure 1)
    help       = dump info                  >> exitWith ExitSuccess
    dump       = hPutStrLn stderr
