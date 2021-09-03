-- Copyright (C) 2021 ash
--
-- This file is part of fsmark.
--
-- fsmark is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- fsmark is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with fsmark.  If not, see <http://www.gnu.org/licenses/>.

module Paths (shelvesPath, Path, shelfPath) where

import Data.Text (Text, pack, unpack)
import qualified System.Directory as DIR
import System.FilePath ((</>))
import qualified Types as T

type Path = Text

shelvesPath :: IO FilePath
shelvesPath = ((</>) "shelves" <$> base)
  where
    base = DIR.getXdgDirectory DIR.XdgData "fsm" >>= \p -> DIR.createDirectoryIfMissing True p >> return p

shelfPath :: T.Shelf -> IO FilePath
shelfPath shelf = (((</>) ((unpack (T.s_name shelf)) ++ ".shelf.json")) <$> shelvesPath)
