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

module Json
  ( entryToJson,
    shelfToJson,
    shelfFromJson,
  )
where

import Control.Exception (throw)
import qualified DB (Context, DBAction, getName, retrieveAll)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Exceptions as EX
import qualified Types as T

data Shelf = Shelf Text [Entry]

data Entry = Entry Text Text

instance FromJSON Entry where
  parseJSON = withObject "Entry" $ \v ->
    Entry
      <$> v .: "name"
      <*> v .: "path"

instance ToJSON Entry where
  toJSON (Entry name path) = object ["name" .= name, "path" .= path]

instance ToJSON Shelf where
  toJSON (Shelf name entries) = object ["name" .= name, "entries" .= entries]

instance FromJSON Shelf where
  parseJSON = withObject "Shelf" $ \v ->
    Shelf <$> v .: "name" <*> v .: "entries"

type JShelf = (T.Shelf, [T.Entry])

makeJEntry tent = Entry (T.name tent) (T.path tent)

makeJShelf shelf ctx =
  DB.getName shelf ctx
    >>= \name ->
      DB.retrieveAll ctx
        >>= \entries -> return $ Shelf name (map makeJEntry entries)

toInternalEntry :: Entry -> T.Entry
toInternalEntry (Entry name path) = T.Entry name path

toInternalShelf :: Shelf -> (T.Shelf, [T.Entry])
toInternalShelf (Shelf name entries) = (T.ShelfName name, makeEntries)
  where
    makeEntries = map toInternalEntry entries

entryToJson :: T.Entry -> ByteString
entryToJson entry = encode $ makeJEntry entry

shelfToJson :: T.Shelf -> DB.Context -> DB.DBAction ByteString
shelfToJson shelf ctx = encode <$> makeJShelf shelf ctx

shelfFromJson :: ByteString -> IO (T.Shelf, [T.Entry])
shelfFromJson j_str = doDecode $ decode j_str
  where
    doDecode :: Maybe Shelf -> IO JShelf
    doDecode mshelf = handleErr mshelf >>= \shelf -> return $ toInternalShelf shelf

    handleErr :: Maybe a -> IO a
    handleErr Nothing = throw $ EX.BadInput ""
    handleErr (Just f) = return f