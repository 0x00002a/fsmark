{-# LANGUAGE FlexibleContexts #-}
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

module ImportExport
    ( Exportable(formatForExport)
    , exportToHandle
    , importFromHandle
    , importShelfFromHandleRaw,
    exportToFile
    )
where

import           Control.Monad                  ( when )
import           Control.Exception              ( throw )
import qualified Control.Exception             as CE
                                                ( handle )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as B
import           Data.Text                      ( Text )
import           Database.SQLite.Simple         ( Error )
import qualified Exceptions                    as EX
import qualified Json                          as J
import           System.IO                      ( Handle )
import qualified Types                         as T
import Data.Aeson (encode, decode)

class Exportable a where
  formatForExport :: a -> ByteString
  importFromText :: B.ByteString -> Maybe a

instance Exportable T.Shelf where
    formatForExport = encode
    importFromText = decode


exportToFile :: (Exportable a) => a -> FilePath -> IO ()
exportToFile target path = B.writeFile path (formatForExport target)

exportToHandle :: (Exportable a) => a -> Handle -> IO ()
exportToHandle target handle = B.hPut handle (formatForExport target)

importFromHandle :: (Exportable a) => Handle -> IO (Maybe a)
importFromHandle handle = importFromText <$> B.hGetContents handle

importShelfFromHandleRaw :: Handle -> IO (Maybe T.Shelf)
importShelfFromHandleRaw = importFromHandle

