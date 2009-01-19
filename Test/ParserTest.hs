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

tests :: Test
tests = TestList [ emptyTest
                 , staticTest
                 , normalTest
                 , pathTest
                 , nameTest ]
