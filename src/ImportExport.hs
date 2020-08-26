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
  ( Exportable (formatForExport),
    exportToHandle,
    importFromHandle,
    importShelfFromHandleRaw,
  )
where

import qualified Control.Exception as CE (handle)
import Control.Monad.Except (liftIO, return, throwError)
import qualified DB
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import Database.SQLite.Simple (Error)
import qualified Exceptions as EX
import qualified Json as J
import System.IO (Handle)
import qualified Types as T

class Exportable a where
  formatForExport :: a -> DB.Context -> IO ByteString
  importFromText :: B.ByteString -> DB.Context -> EX.Exception IO a

instance Exportable T.Shelf where
  formatForExport = J.shelfToJson
  importFromText txt db_ctx =
    J.shelfFromJson txt
      >>= \(shelf, entries) -> checkNotExists shelf >> (liftIO $ DB.insert shelf db_ctx >> DB.insertMany entries (dest_ctx shelf) >> return shelf)
    where
      checkNotExists :: T.Shelf -> EX.Exception IO ()
      checkNotExists shelf@(T.ShelfName shelf_name) =
        liftIO (DB.exists shelf db_ctx) >>= \exists ->
          if exists
            then throwError $ EX.NamingConflict $ EX.Shelf shelf_name
            else return ()
      dest_ctx shelf = DB.changeTargetShelf shelf db_ctx

exportToFile :: (Exportable a) => a -> DB.Context -> FilePath -> IO ()
exportToFile target ctx path = formatForExport target ctx >>= B.writeFile path

exportToHandle :: (Exportable a) => a -> DB.Context -> Handle -> IO ()
exportToHandle target ctx handle = formatForExport target ctx >>= B.hPut handle

importFromHandle :: (Exportable a) => Handle -> DB.Context -> EX.Exception IO a
importFromHandle handle db_ctx =
  liftIO (B.hGetContents handle)
    >>= \content -> importFromText content db_ctx

importShelfFromHandleRaw :: Handle -> EX.Exception IO (T.Shelf, [T.Entry])
importShelfFromHandleRaw handle = liftIO (B.hGetContents handle) >>= J.shelfFromJson