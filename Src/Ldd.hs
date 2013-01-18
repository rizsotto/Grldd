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

module Ldd
        ( parse'
        , getDependencies
        ) where


import System.IO (hGetContents)
import System.Exit (ExitCode(..))
import System.Process
import Text.ParserCombinators.Parsec

---------- dependency lookup with exec ldd
getDependencies :: FilePath -> IO [FilePath]
getDependencies fn = do
        (_, Just outh, Just errh, pid) <-
            createProcess (proc "ldd" [fn]) { std_out = CreatePipe
                                            , std_err = CreatePipe }
        out <- hGetContents outh
        err <- hGetContents errh
        exit <- waitForProcess pid
        case exit of
            ExitSuccess -> case parse' fn out of
                Right result -> return $ filter (not . null) result
                Left e -> fail ("Internal error: " ++ show e)
            _ -> fail err

---------- parse ldd output
--
-- possible output of successful running
--
--    statically linked
--    path (address)
--    name => path (address)
--    name => (address)

hexadecimal :: Parser ()
hexadecimal = do { _ <- char '0'
                 ; _ <- char 'x'
                 ; _ <- many1 hexDigit
                 ; return ()
                 }

address :: Parser ()
address = do { _ <- char '('
             ; _ <- hexadecimal
             ; _ <- char ')'
             ; return ()
             }

filechar :: Parser Char
filechar = alphaNum <|> oneOf ".,_-+"

filename :: Parser String
filename = many1 filechar

separator :: Parser Char
separator = char '/'

path :: Parser String
path = do { _ <- separator
          ; f <- sepBy filename separator
          ; return $ foldr (\w res -> '/' : w ++ res) "" f
          }

arrow :: Parser ()
arrow = do { spaces
           ; _ <- string "=>"
           ; spaces
           ; return ()
           }

entry :: Parser FilePath
entry =   do { _ <- string "statically linked"
             ; return ""
             }
        <|>
          do { _ <- try path 
             ; spaces
             ; address
             ; return ""
             }
        <|>
          do { _ <- filename
             ; arrow
             ; do { p <- try path
                  ; spaces
                  ; address
                  ; return p
                  }
               <|> do { address
                      ; return ""
                      }
             }

line :: Parser FilePath
line = do { spaces
          ; entry
          }
       <|> return ""

eol :: Parser Char
eol = char '\n'

ldd :: Parser [FilePath]
ldd = sepBy line eol

parse' :: String -> String -> Either ParseError [FilePath]
parse' file = parse ldd ("(ldd output on " ++ file ++ ")")
