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

module Json where

import Control.Exception (throw)
import Data.Aeson
    ( object,
      (.:),
      withObject,
      KeyValue((.=)),
      ToJSON(toJSON),
      FromJSON(parseJSON) )
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Exceptions as EX
import qualified Types as T

instance FromJSON T.Entry where
  parseJSON = withObject "Entry" $ \v ->
    T.Entry
      <$> v .: "name"
      <*> v .: "path"

instance ToJSON T.Entry where
  toJSON (T.Entry name path) = object ["name" .= name, "path" .= path]

instance ToJSON T.Shelf where
  toJSON (T.Shelf name entries) = object ["name" .= name, "entries" .= entries]

instance FromJSON T.Shelf where
  parseJSON = withObject "Shelf" $ \v ->
    T.Shelf <$> v .: "name" <*> v .: "entries"
