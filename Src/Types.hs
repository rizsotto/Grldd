module Types
        ( SoInfo
        , empty
        , isEmpty
        ) where

type SoInfo = (String, FilePath)

empty :: SoInfo
empty = ("", "")

isEmpty :: SoInfo -> Bool
isEmpty (name, path) = (name == "") || (path == "")