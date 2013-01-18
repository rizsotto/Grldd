{-  Grldd is a dependency tracking tool.
    Copyright (C) 2009-2013  Laszlo Nagy

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

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
emptyTest = TestCase (assertEqual "" expected result)
  where
    tv = []
    expected = M.fromList tv
    result = runTest tv []

noArgTest = TestCase (assertEqual "" expected result)
  where
    tv = [(1, ([],"a"))]
    expected = M.fromList []
    result = runTest tv []

singleTest = TestCase (assertEqual "" expected result)
  where
    tv = [(1, ([],"a"))]
    expected = M.fromList tv
    result = runTest tv [1]

noRouteTest = TestCase (assertEqual "" expected result)
  where
    tv = [(1, ([ ],"a")),
          (2, ([1],"a"))]
    expected = M.fromList [(1, ([],"a"))]
    result = runTest tv [1]

simpleTest = TestCase (assertEqual "" expected result)
  where
    tv = [(1, ([2,3],"a")),
          (2, ([1,3],"a")),
          (3, ([4,5],"b")),
          (4, ([3],  "c")),
          (5, ([],   "d"))]
    expected = M.fromList tv
    result = runTest tv [1]

twoArgsTest = TestCase (assertEqual "" expected result)
  where
    tv = [(1, ([2,3],"a")),
          (2, ([1,3],"a")),
          (3, ([4,5],"b")),
          (4, ([3],  "c")),
          (5, ([],   "d")),
          (6, ([5],  "e"))]
    expected = M.fromList tv
    result = runTest tv [1, 6]


tests = TestList [ emptyTest
                 , noArgTest
                 , singleTest
                 , simpleTest
                 , noRouteTest
                 , twoArgsTest
                 ]
