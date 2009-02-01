module ParserTest (tests) where

import Types
import Ldd.Parser

import Text.Parsec.Error
import Control.Monad (unless)

import Test.HUnit


assertEmpty :: Either ParseError [SoInfo] -> Assertion
assertEmpty value = case value of
    Right [x] -> unless (isEmpty x) (assertFailure $ message x)
    Right _ -> assertFailure "Expected one empty, but got more than one"
    Left _  -> assertFailure "Unexpected parse error\n"
  where
    message x = "Expected empty, but got: "++ show x ++ "\n"


assertSoInfo :: [SoInfo] -> Either ParseError [SoInfo] -> Assertion
assertSoInfo expected actual = case actual of
    Right x -> unless (x == expected) (assertFailure $ message x)
    Left _  -> assertFailure "Unexpected parse error\n"
  where
    message x = "Expected: " ++ show expected ++ ", but got: "++ show x ++ "\n"



emptyTest :: Test
emptyTest = TestCase (assertEmpty result)
    where
        result   = parseLdd "" ""

staticTest :: Test
staticTest = TestCase (assertEmpty result)
    where
        result   = parseLdd "" "\tstatically linked"

normalTest :: Test
normalTest = TestCase (assertSoInfo expected result)
    where
        expected = [(name, path)]
        result   = parseLdd "" ("\t" ++ name ++ " => " ++ path ++ " (0xaa)")
        name = "liba.so.7"
        path = "/path/" ++ name

pathTest :: Test
pathTest = TestCase (assertEmpty result)
    where
        result   = parseLdd "" "\t/path/liba.so.9 (0xaa)"

nameTest :: Test
nameTest = TestCase (assertEmpty result)
    where
        result   = parseLdd "" "\tliba.so.8 => (0xaa)"

realTest :: Test
realTest = TestCase (assertSoInfo expected result)
    where
        expected = [ empty,
                    ("libdl.so.2", "/lib/libdl.so.2"),
                    ("libc.so.6", "/lib/libc.so.6"),
                     empty ]
        result   = parseLdd ""
                    ("\tlinux-gate.so.1 =>  (0xb7f17000)\n" ++
                     "\tlibdl.so.2 => /lib/libdl.so.2 (0xb7ef4000)\n" ++
                     "\tlibc.so.6 => /lib/libc.so.6 (0xb7d96000)\n" ++
                     "\t/lib/ld-linux.so.2 (0xb7f18000)")

filenameTest :: Test
filenameTest = TestCase (assertEqual "basename" expected result)
    where
        expected = "libpam.so.0"
        result = parsePath "/lib/libpam.so.0"

tests :: Test
tests = TestList [ emptyTest
                 , staticTest
                 , normalTest
                 , pathTest
                 , nameTest
                 , realTest
                 , filenameTest ]
