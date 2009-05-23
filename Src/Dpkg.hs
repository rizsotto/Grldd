module Dpkg
        ( Package
        , getPackage
        ) where

type Package = String

getPackage :: FilePath -> IO (Maybe Package)
getPackage _ = return Nothing
