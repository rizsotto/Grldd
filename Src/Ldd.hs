module Ldd
        ( parse'
        , getDependencies
        ) where


import System.IO (hGetContents, hClose)
import System.Exit (ExitCode(..))
import System.Process
import Text.ParserCombinators.Parsec


getDependencies :: FilePath -> IO ([FilePath])
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

parse' :: String -> String -> Either ParseError [FilePath]
parse' file = parse ldd ("(ldd output on " ++ file ++ ")")
