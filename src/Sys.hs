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

module Sys
    ( OS(..)
    , escapePath
    , osFromStr
    , os
    )
where

import qualified System.Info                   as SI
import qualified System.Directory              as DIR


data OS = Windows | Unix | Unknown deriving(Eq, Show)


escapePath :: FilePath -> OS -> FilePath
escapePath path platform = concatMap (`escapeSegment` platform) path



escapeSegment :: Char -> OS -> FilePath
escapeSegment ' ' Windows = "^ "
escapeSegment ' ' Unix    = "\\ "
escapeSegment ' ' Unknown = "\" \""
escapeSegment ch  _       = [ch]

os :: OS
os = osFromStr SI.os


osFromStr :: String -> OS
osFromStr "mingw32" = Windows
osFromStr "linux"   = Unix
osFromStr "darwin"  = Unix
osFromStr _         = Unknown

expandPath :: Filepath -> IO FilePath
expandPath ['~' : fp] = ((++) fp) <$> Dir.getHomeDirectory
expandPath fp         = return fp
