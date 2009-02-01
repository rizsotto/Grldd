module Ldd
        ( getDependencies
        , parsePath
        ) where

import Types
import Ldd.Parser

import System.IO (hGetContents, hClose)
import System.Exit (ExitCode(..))
import System.Process ( createProcess
                      , proc
                      , std_out
                      , std_err
                      , StdStream(..)
                      , waitForProcess)
import System.Posix.Files (fileExist)


getDependencies :: FilePath -> IO [SoInfo]
getDependencies fn = do
        accesible <- fileExist fn
        if accesible
            then do
                (_, Just outh, Just errh, pid) <-
                    createProcess (proc "ldd" [fn])
                        { std_out = CreatePipe, std_err = CreatePipe }
                out <- hGetContents outh
                case parseLdd fn out of
                    Right result -> do
                        exit <- waitForProcess pid
                        hClose outh
                        case exit of
                            ExitSuccess -> return $ filter (not . isEmpty) result
                            _           -> fail "Problem with execute ldd."
                    Left err -> fail ("Internal error: " ++ show err)
            else fail ("File is not exist: " ++ fn)
