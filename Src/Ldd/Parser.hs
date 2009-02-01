module Ldd.Parser
        ( parseLdd
        , parsePath
        ) where

import Types

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

entry :: Parser SoInfo
entry =   do { string "statically linked"
             ; return empty }
        <|>
          do { p <- try path 
             ; spaces
             ; address
             ; return empty }
        <|>
          do { n <- filename
             ; arrow
             ; do { p <- try path
                  ; spaces
                  ; address
                  ; return (n, p) }
               <|> do { address
                      ; return empty }}

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