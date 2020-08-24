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

module FSM where

import Control.Monad.Except (liftIO, throwError)
import qualified DB
import Data.Text (Text, append)
import Exceptions (Error (BadInput, DoesNotExist), Exception)
import qualified Exceptions as EX
import System.IO (Handle)
import qualified Types as T

addEntry :: T.Entry -> DB.Context -> Exception IO ()
addEntry entry db_ctx = checkFileNotExists entry db_ctx >> liftIO (DB.insert entry db_ctx)

copyEntry :: T.Shelf -> T.Shelf -> Text -> DB.Context -> Exception IO ()
copyEntry from to entry db_ctx =
  if from == to
    then throwError $ BadInput "Source and destination shelves must be different"
    else existsCheck >> (\src -> doCopy to (DB.changeTargetShelf src db_ctx)) from
  where
    doCopy = \to src_ctx -> DB.copyEntry from to entry db_ctx
    existsCheck = checkShelfExists from db_ctx >> checkShelfExists to db_ctx

getAllEntries :: DB.Context -> Maybe Text -> IO [T.Entry]
getAllEntries db_ctx (Just name_filter) = DB.retrieveAllLike name_filter db_ctx
getAllEntries db_ctx Nothing = DB.retrieveAll db_ctx

checkShelfExists :: T.Shelf -> DB.Context -> Exception IO ()
checkShelfExists shelf db_ctx =
  (liftIO $ DB.exists shelf db_ctx)
    >>= \exists ->
      if exists
        then return ()
        else throwError $ EX.DoesNotExist $ EX.Shelf $ getShelfName shelf
  where
    getShelfName (T.ShelfName n) = n
    getShelfName (T.ShelfID _) = "Unknown"

renameShelfByName :: Text -> Text -> DB.Context -> Exception IO ()
renameShelfByName from to db_ctx = checkShelfExists shelf db_ctx >> (liftIO $ DB.rename shelf to db_ctx)
  where
    shelf = T.ShelfName from

removeEntryByName :: Text -> DB.Context -> Exception IO ()
removeEntryByName entry db_ctx =
  checkEntryExists (T.Entry entry "" (DB.target_shelf db_ctx)) db_ctx
    >> (liftIO $ DB.removeFile entry db_ctx)

renameEntryByName :: Text -> Text -> DB.Context -> Exception IO ()
renameEntryByName from to db_ctx =
  checkEntryExistsByName from db_ctx >> liftIO (DB.getFiles from db_ctx)
    >>= \files -> liftIO $ DB.rename (files !! 0) to db_ctx

checkEntryExistsByName :: Text -> DB.Context -> Exception IO ()
checkEntryExistsByName entry_name ctx = checkEntryExists (T.Entry entry_name "" (DB.target_shelf ctx)) ctx

checkEntryExists :: T.Entry -> DB.Context -> Exception IO ()
checkEntryExists entry ctx =
  (liftIO $ DB.exists entry ctx)
    >>= \exists ->
      if exists
        then return ()
        else throwError $ DoesNotExist $ EX.Entry $ T.name entry

checkFileNotExists :: T.Entry -> DB.Context -> Exception IO ()
checkFileNotExists entry ctx =
  (liftIO $ DB.exists entry ctx)
    >>= \exists ->
      if exists
        then throwError $ EX.NamingConflict $ EX.Entry (T.name entry)
        else return ()

checkShelfNotExists :: T.Shelf -> DB.Context -> Exception IO ()
checkShelfNotExists shelf ctx =
  (liftIO $ DB.exists shelf ctx)
    >>= \exists ->
      if exists
        then throwError $ EX.NamingConflict $ EX.Shelf $ shelfDecider shelf
        else return ()
  where
    shelfDecider (T.ShelfName n) = n
    shelfDecider (T.ShelfID _) = "Unknown"

addShelf :: T.Shelf -> DB.Context -> Exception IO ()
addShelf shelf db_ctx = checkShelfNotExists shelf db_ctx >> (liftIO $ DB.insert shelf db_ctx)

setupFile :: Text -> DB.Context -> T.Entry
setupFile name ctx = T.Entry name "" (DB.target_shelf ctx)

connectToDb :: Maybe T.Shelf -> Maybe Text -> Exception IO DB.Context
connectToDb (Just shelf@(T.ShelfName shelf_name)) db_path = liftIO (DB.connect shelf_name db_path) >>= \db -> checkShelfExists shelf db >> return db
connectToDb Nothing db_path = liftIO $ DB.connect DB.defaultShelfName db_path

moveEntryByName :: Text -> Text -> Text -> DB.Context -> Exception IO ()
moveEntryByName from to name ctx = copyEntry (T.ShelfName from) (T.ShelfName to) name ctx >> FSM.removeEntryByName name ctx
