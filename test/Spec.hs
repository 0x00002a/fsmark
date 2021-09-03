-- Copyright (C) 2020 Natasha England-Elbro
--
-- This file is part of file-shelf.
--
-- file-shelf is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- file-shelf is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with file-shelf.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Except
  ( liftIO,
    runExceptT,
  )
import qualified DB
import Data.Text
  ( Text,
    pack,
  )
import qualified Exceptions as EX
import qualified FSM
import qualified Frontend as F
import qualified Sys
import qualified System.Directory as DIR
import Test.HUnit
import qualified Types as T



escapePathsTest = TestCase $ winTest >> unixTest >> unknownTest
  where
    winTest =
      assertEqual
        "Windows path escaped"
        "/home^ /test"
        (Sys.escapePath "/home /test" Sys.Windows)
    unixTest =
      assertEqual
        "Unix path escaped"
        "/home\\ /test"
        (Sys.escapePath "/home /test" Sys.Unix)
    unknownTest =
      assertEqual
        "Unknown system path escaped"
        "\"/home /test\""
        (Sys.escapePath "/home /test" Sys.Unknown)

osFromStrTest = TestCase $ winTest >> linuxTest >> macTest >> unknownTest
  where
    winTest =
      assertEqual "Windows OS is correct" Sys.Windows $
        Sys.osFromStr "mingw32"
    linuxTest =
      assertEqual "Linux OS is correct" Sys.Unix $ Sys.osFromStr "linux"
    macTest = assertEqual "Mac OS is correct" Sys.Unix $ Sys.osFromStr "darwin"
    unknownTest =
      assertEqual "Unknown OS is detected" Sys.Unknown $
        Sys.osFromStr "obscureOsName"

expandPathTest =
  TestCase $
    homeDir >>= \home ->
      Sys.expandPath "~/file.txt" >>= \expanded ->
        assertEqual "Path expands properly" (home ++ "/file.txt") expanded
  where
    homeDir = DIR.getHomeDirectory

tests =
  TestList
    [ TestLabel "Escaping paths works" escapePathsTest,
      TestLabel "Reading OS from string works" osFromStrTest,
      TestLabel "Expanding paths works" expandPathTest
    ]

main :: IO Counts
main = runTestTT tests
