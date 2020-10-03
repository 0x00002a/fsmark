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
    , importShelfFromHandleRaw
    )
where

import           Control.Monad                  ( when )
import           Control.Exception              ( throw )
import qualified Control.Exception             as CE
                                                ( handle )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import qualified DB
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as B
import           Data.Text                      ( Text )
import           Database.SQLite.Simple         ( Error )
import qualified Exceptions                    as EX
import qualified Json                          as J
import           System.IO                      ( Handle )
import qualified Types                         as T

class Exportable a where
  formatForExport :: a -> DB.Context -> DB.DBAction ByteString
  importFromText :: B.ByteString -> DB.Context -> DB.DBAction a

instance Exportable T.Shelf where
    formatForExport = J.shelfToJson
    importFromText txt db_ctx =
        liftIO (J.shelfFromJson txt) >>= \(shelf, entries) ->
            checkNotExists shelf
                >> (  DB.insert shelf db_ctx
                   >> DB.insertMany entries (dest_ctx shelf)
                   >> return shelf
                   )
      where
        checkNotExists shelf@(T.ShelfName shelf_name) =
            DB.exists shelf db_ctx
                >>= \exists ->
                        when exists $ throw $ EX.NamingConflict $ EX.Shelf
                            shelf_name
        dest_ctx shelf = DB.changeTargetShelf shelf db_ctx

exportToFile :: (Exportable a) => a -> DB.Context -> FilePath -> DB.DBAction ()
exportToFile target ctx path = formatForExport target ctx >>= B.writeFile path

exportToHandle :: (Exportable a) => a -> DB.Context -> Handle -> DB.DBAction ()
exportToHandle target ctx handle =
    formatForExport target ctx >>= \i -> liftIO $ B.hPut handle i

importFromHandle :: (Exportable a) => Handle -> DB.Context -> DB.DBAction a
importFromHandle handle db_ctx =
    B.hGetContents handle >>= \content -> importFromText content db_ctx

importShelfFromHandleRaw :: Handle -> DB.DBAction (T.Shelf, [T.Entry])
importShelfFromHandleRaw handle = B.hGetContents handle >>= J.shelfFromJson

