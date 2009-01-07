module Ldd
        ( SoInfo
        , getDependencies
        , parsePath
        ) where

import System.IO (hGetContents, hClose)
import System.Exit (ExitCode(..))
import System.Process ( createProcess
                      , proc
                      , std_out
                      , StdStream(..)
                      , waitForProcess)
import System.Posix.Files (fileExist)
import Text.ParserCombinators.Parsec
import Control.Exception (throw, SomeException(..))

type SoInfo = (String, FilePath)

getDependencies :: FilePath -> IO [SoInfo]
getDependencies fn = do
        accesible <- fileExist fn
        if accesible
            then do
                (_, Just outh, _, pid) <-
                    createProcess (proc "ldd" [fn]){ std_out = CreatePipe }
                out <- hGetContents outh
                case parseLdd fn out of
                    Right result -> do
                        exit <- waitForProcess pid
                        hClose outh
                        case exit of
                            ExitSuccess -> return $ filter (not . empty) result
                            _           -> fail "Problem with execute ldd."
                    Left err -> fail ("Internal error!\n" ++ show err)
            else fail ("File is not exist: " ++ fn)
    where
        empty :: SoInfo -> Bool
        empty (name, path) = (name == "") || (path == "")

-- parse ldd output
--    statically linked
--    path (address)
--    name => path (address)
--    name => (address)

hexadecimal :: Parser ()
hexadecimal = do { char '0'
                 ; char 'x'
                 ; many1 hexDigit
                 ; return () }

address :: Parser ()
address = do { char '('
             ; hexadecimal
             ; char ')'
             ; return () }

filechar :: Parser Char
filechar = alphaNum <|> oneOf ".,_-+"

filename :: Parser String
filename = many1 filechar

separator :: Parser Char
separator = char '/'

path :: Parser String
path = do { separator
          ; f <- sepBy filename separator
          ; return $ foldr (\w res -> '/' : w ++ res) "" f }

arrow :: Parser ()
arrow = do { spaces
           ; string "=>"
           ; spaces
           ; return () }

entry :: Parser SoInfo
entry =   do { string "statically linked"
             ; return ("", "") }
        <|>
          do { p <- try path 
             ; spaces
             ; address
             ; return ("", p) }
        <|>
          do { n <- filename
             ; arrow
             ; do { p <- try path
                  ; spaces
                  ; address
                  ; return (n, p) }
               <|> do { address
                      ; return (n, "") }}

line :: Parser SoInfo
line = do { spaces
          ; e <- entry
          ; return e }
        <|> return ("", "")

eol = char '\n'
ldd = sepBy line eol

parseLdd :: String -> String -> Either ParseError [SoInfo]
parseLdd file = parse ldd ("(ldd output on " ++ file ++ ")")


path' :: Parser String
path' = do { separator
           ; f <- sepBy filename separator
           ; return $ last f }

parsePath :: String -> String
parsePath = do
        result <- parse path' ""
        case result of
            Right p -> return p
            Left _  -> fail "Wrong file name: "