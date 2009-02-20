module Ldd
        ( getDependencies
        , Package
        , getPackage
        ) where

import Ldd.Parser

import System.IO (hGetContents, hClose)
import System.Exit (ExitCode(..))
import System.Process


getDependencies :: FilePath -> IO ([FilePath])
getDependencies fn = do
        (_, Just outh, Just errh, pid) <-
            createProcess (proc "ldd" [fn]) { std_out = CreatePipe
                                            , std_err = CreatePipe }
        out <- hGetContents outh
        err <- hGetContents errh
        exit <- waitForProcess pid
        case exit of
            ExitSuccess -> case parseLdd fn out of
                Right result -> return $ filter (not . null) result
                Left e -> fail ("Internal error: " ++ show e)
            _ -> fail err


type Package = String

getPackage :: FilePath -> IO (Maybe Package)
getPackage fn = return Nothing
