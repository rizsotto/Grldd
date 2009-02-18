{- LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances -}
module Ldd
        ( SharedObject(..)
        , Package
        , Message
        ) where

import Ldd.Parser

import System.IO (hGetContents, hClose)
import System.Exit (ExitCode(..))
import System.Process


type Package = String
type Message = String

class Monad m => SharedObject m where
    getDependencies :: FilePath -> m (Either Message [FilePath])
    getPackage      :: FilePath -> m (Maybe Package)


getDependencies' :: FilePath -> IO (Either Message [FilePath])
getDependencies' fn = do
        (_, Just outh, Just errh, pid) <-
            createProcess (proc "ldd" [fn]) { std_out = CreatePipe
                                            , std_err = CreatePipe }
        out <- hGetContents outh
        err <- hGetContents errh
        exit <- waitForProcess pid
        case exit of
            ExitSuccess -> case parseLdd fn out of
                Right result -> return $ Right $ filter (not . null) result
                Left e -> return $ Left ("Internal error: " ++ show e)
            _ -> return $ Left err

getPackage' :: FilePath -> IO (Maybe Package)
getPackage' fn = return Nothing


instance SharedObject IO where
    getDependencies = getDependencies'
    getPackage = getPackage'