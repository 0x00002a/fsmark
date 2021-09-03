-- Copyright (C) 2020 Natasha England-Elbro
--
-- This file is part of file-shelf.
--
-- file-shelf is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- file-shelf is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
-- A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along with
-- file-shelf.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE OverloadedStrings #-}

module DB where
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text, isSuffixOf, pack)
import qualified Data.Text as TE
import qualified ImportExport as EXP
import Json
import qualified Paths as P
import qualified System.Directory as DIR
import qualified Types as T

allShelves :: IO [Maybe T.Shelf]
allShelves =
  P.shelvesPath
    >>= DIR.listDirectory
    >>= \dirs -> mapM (\p -> loadShelfFromName $ pack p) (filter (\p -> isSuffixOf P.shelfExtension (pack p)) dirs)

saveShelf :: T.Shelf -> Maybe FilePath -> IO ()
saveShelf shelf Nothing = (Just <$> P.shelfPath shelf) >>= saveShelf shelf
saveShelf shelf (Just path) = EXP.exportToFile shelf path

saveShelfDefault shelf = saveShelf shelf Nothing

shelfExists :: Text -> IO Bool
shelfExists name = P.shelfPath (T.Shelf name []) >>= DIR.doesFileExist

loadShelfFromName :: Text -> IO (Maybe T.Shelf)
loadShelfFromName name = decode <$> loadData
  where
    loadData :: IO B.ByteString
    loadData = P.shelfPath (T.Shelf name []) >>= B.readFile
removeShelf :: Text -> IO ()
removeShelf name = P.shelfPath (T.Shelf name []) >>= DIR.removeFile
