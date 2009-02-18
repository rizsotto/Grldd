module Main where

import ParserTest (tests)
import GraphTest  (tests)

import Test.HUnit (runTestTT)

main = do
        runTestTT $ ParserTest.tests
        runTestTT $ GraphTest.tests
