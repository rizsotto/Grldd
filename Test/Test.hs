module Main where

import ParserTest

import Test.HUnit

main = do
        runTestTT $ ParserTest.tests