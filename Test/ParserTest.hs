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

module ParserTest (tests) where

import Ldd

import Control.Monad (unless)

import Test.HUnit


---------- test fixtures
assertEmpty :: Either a [FilePath] -> Assertion
assertEmpty value = case value of
    Right [x] -> unless (null x) (assertFailure $ message x)
    Right _ -> assertFailure "Expected one empty, but got more than one"
    Left _  -> assertFailure "Unexpected parse error\n"
  where
    message x = "Expected empty, but got: "++ show x ++ "\n"

assertSoInfo :: [FilePath] -> Either a [FilePath] -> Assertion
assertSoInfo expected actual = case actual of
    Right x -> unless (x == expected) (assertFailure $ message x)
    Left _  -> assertFailure "Unexpected parse error\n"
  where
    message x = "Expected: " ++ show expected ++ ", but got: "++ show x ++ "\n"

--parse :: String -> Either ParseError [FilePath]
parse = parse' "test literal"

---------- test cases
emptyTest = TestCase (assertEmpty result)
    where
        result   = parse ""

staticTest = TestCase (assertEmpty result)
    where
        result   = parse "\tstatically linked"

normalTest = TestCase (assertSoInfo expected result)
    where
        expected = ["/path/liba.so.7"]
        result   = parse "\tliba.so.7 => /path/liba.so.7 (0xaa)"

pathTest = TestCase (assertEmpty result)
    where
        result   = parse "\t/path/liba.so.9 (0xaa)"

nameTest = TestCase (assertEmpty result)
    where
        result   = parse "\tliba.so.8 => (0xaa)"

realTest = TestCase (assertSoInfo expected result)
    where
        expected = ["",
                    "/lib/libdl.so.2",
                    "/lib/libc.so.6",
                    "" ]
        result   = parse
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
                 , realTest
                 ]
