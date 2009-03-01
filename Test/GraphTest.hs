module GraphTest (tests) where

import Graph

import Control.Monad.Reader
import qualified Data.Map as M

import Test.HUnit


---------- test fixtures 
type TestVector = ([(Integer, ([Integer], String))])
type TestSupply = Reader TestVector

getDependencies :: Integer -> TestSupply (Moves Integer String)
getDependencies i = do
        tv <- ask
        case lookup i tv of
            Nothing -> fail "Required dependency is not in test vector."
            Just x  -> return x

runTest :: TestVector -> [Integer] -> Map Integer String
runTest tv starts = runReader (bfsM getDependencies M.empty starts) tv


---------- test cases
empty = TestCase (assertEqual "" expected result)
  where
    tv = []
    expected = M.fromList tv
    result = runTest tv []

noArg = TestCase (assertEqual "" expected result)
  where
    tv = [(1, ([],"a"))]
    expected = M.fromList []
    result = runTest tv []

single = TestCase (assertEqual "" expected result)
  where
    tv = [(1, ([],"a"))]
    expected = M.fromList tv
    result = runTest tv [1]

simple = TestCase (assertEqual "" expected result)
  where
    tv = [(1, ([2,3],"a")),
          (2, ([1,3],"a")),
          (3, ([4,5],"b")),
          (4, ([3],  "c")),
          (5, ([],   "d"))]
    expected = M.fromList tv
    result = runTest tv [1]

twoArgs = TestCase (assertEqual "" expected result)
  where
    tv = [(1, ([2,3],"a")),
          (2, ([1,3],"a")),
          (3, ([4,5],"b")),
          (4, ([3],  "c")),
          (5, ([],   "d")),
          (6, ([5],  "e"))]
    expected = M.fromList tv
    result = runTest tv [1, 6]


tests = TestList [ empty
                 , noArg
                 , single
                 , simple
                 , twoArgs ]
