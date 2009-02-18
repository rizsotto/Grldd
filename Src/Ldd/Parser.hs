module Ldd.Parser
        ( parseLdd
        ) where

import Text.ParserCombinators.Parsec

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

entry :: Parser FilePath
entry =   do { string "statically linked"
              ; return ""}
        <|>
          do { p <- try path 
             ; spaces
             ; address
             ; return ""}
        <|>
          do { n <- filename
             ; arrow
             ; do { p <- try path
                  ; spaces
                  ; address
                  ; return p }
               <|> do { address
                      ; return ""}}

line :: Parser FilePath
line = do { spaces
           ; e <- entry
           ; return e }
        <|> return ""

eol = char '\n'

ldd = sepBy line eol

parseLdd :: String -> String -> Either ParseError [FilePath]
parseLdd file = parse ldd ("(ldd output on " ++ file ++ ")")
