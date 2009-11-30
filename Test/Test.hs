module Main where

import Test.HUnit (Test(..))

import ParserTest (tests)
import GraphTest  (tests)

import Test.HUnit (runTestTT)

main = do
        runTestTT $ TestList
			[ ParserTest.tests
			, GraphTest.tests
			]
