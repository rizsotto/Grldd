module ParserTest (tests) where

import Ldd

import Text.Parsec.Error
import Control.Monad (unless)

import Test.HUnit


tsrc :: String
tsrc = "test source"

assertEmpty :: Either ParseError [FilePath] -> Assertion
assertEmpty value = case value of
    Right [x] -> unless (null x) (assertFailure $ message x)
    Right _ -> assertFailure "Expected one empty, but got more than one"
    Left _  -> assertFailure "Unexpected parse error\n"
  where
    message x = "Expected empty, but got: "++ show x ++ "\n"


assertSoInfo :: [FilePath] -> Either ParseError [FilePath] -> Assertion
assertSoInfo expected actual = case actual of
    Right x -> unless (x == expected) (assertFailure $ message x)
    Left _  -> assertFailure "Unexpected parse error\n"
  where
    message x = "Expected: " ++ show expected ++ ", but got: "++ show x ++ "\n"

emptyTest :: Test
emptyTest = TestCase (assertEmpty result)
    where
        result   = parse' tsrc ""

staticTest :: Test
staticTest = TestCase (assertEmpty result)
    where
        result   = parse' tsrc "\tstatically linked"

normalTest :: Test
normalTest = TestCase (assertSoInfo expected result)
    where
        expected = ["/path/liba.so.7"]
        result   = parse' tsrc ("\tliba.so.7 => /path/liba.so.7 (0xaa)")

pathTest :: Test
pathTest = TestCase (assertEmpty result)
    where
        result   = parse' tsrc "\t/path/liba.so.9 (0xaa)"

nameTest :: Test
nameTest = TestCase (assertEmpty result)
    where
        result   = parse' tsrc "\tliba.so.8 => (0xaa)"

realTest :: Test
realTest = TestCase (assertSoInfo expected result)
    where
        expected = ["",
                    "/lib/libdl.so.2",
                    "/lib/libc.so.6",
                    "" ]
        result   = parse' tsrc
                    ("\tlinux-gate.so.1 =>  (0xb7f17000)\n" ++
                     "\tlibdl.so.2 => /lib/libdl.so.2 (0xb7ef4000)\n" ++
                     "\tlibc.so.6 => /lib/libc.so.6 (0xb7d96000)\n" ++
                     "\t/lib/ld-linux.so.2 (0xb7f18000)")

tests :: Test
tests = TestList [ emptyTest
                 , staticTest
                 , normalTest
                 , pathTest
                 , nameTest
                 , realTest ]
