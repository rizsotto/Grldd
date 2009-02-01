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
                exit <- waitForProcess pid
                hClose errh
                case exit of
                    ExitSuccess -> case parseLdd fn out of
                        Right result -> do
                                  hClose outh
                                  return $ filter (not . isEmpty) result
                        Left err -> fail ("Internal error: " ++ show err)
                    _ -> fail "Problem with execute ldd."
            else fail ("File is not exist: " ++ fn)
