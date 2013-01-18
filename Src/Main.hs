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

module Main where

import Graph

import Prelude hiding (catch)
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Console.GetOpt
import Control.Exception


data Flag = Help
  deriving Eq

options :: [OptDescr Flag]
options = [Option ['h'] ["help"] (NoArg Help) "Show this help message"]


main :: IO ()
main =
    catch (parseArgs >>= inspect >>= print')
          (\(e :: IOException) -> do
                    dump ("grldd: failed " ++ show e)
                    exitFailure)
  where
    parseArgs :: IO [String]
    parseArgs = do
            argv <- getArgs
            case parse argv of
                        ([], [], [])     -> help
                        ([], files, [])  -> return files
                        (_,_,errs)       -> die errs

    parse      = getOpt Permute options
    header     = "Usage: grldd [-h] [file ...]"
    info       = usageInfo header options
    die errs   = dump (concat errs ++ info) >> exitFailure
    help       = dump info                  >> exitSuccess
    dump       = hPutStrLn stderr
