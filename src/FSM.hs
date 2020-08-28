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

import Control.Monad.Except (lift, liftIO, liftM, runExceptT, throwError)
import qualified DB
import Data.Text (Text, append, pack)
import Exceptions (Error (BadInput, DoesNotExist), Exception)
import qualified Exceptions as EX
import qualified ImportExport as IE
import qualified Json as J
import qualified System.Directory as DIR
import qualified System.FilePath as FP
import System.IO
  ( FilePath,
    Handle,
    IOMode (ReadMode, WriteMode),
    hClose,
    openFile,
    stdin,
    stdout,
    withFile,
  )
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

removeItem :: (DB.DBObject a) => a -> DB.Context -> Exception IO ()
removeItem item db_ctx = checkExists item db_ctx >> (liftIO $ DB.remove item db_ctx)

removeShelfByName :: Text -> DB.Context -> Exception IO ()
removeShelfByName name = removeItem (T.ShelfName name)

renameEntryByName :: Text -> Text -> DB.Context -> Exception IO ()
renameEntryByName from to db_ctx =
  checkEntryExistsByName from db_ctx >> liftIO (DB.getFiles from db_ctx)
    >>= \files -> liftIO $ DB.rename (files !! 0) to db_ctx

checkExists :: (DB.DBObject a) => a -> DB.Context -> Exception IO ()
checkExists item db_ctx = liftIO (DB.exists item db_ctx) >>= doCheck
  where
    doCheck :: Bool -> Exception IO ()
    doCheck does_exist =
      if does_exist
        then return ()
        else
          liftIO (DB.getName item db_ctx)
            >>= \name -> throwError $ EX.TextError $ name `append` " does not exist"

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
moveEntryByName from to name ctx = copyEntry (T.ShelfName from) (T.ShelfName to) name ctx >> removeEntryByName name ctx

withOutputHandle :: Maybe FilePath -> (Handle -> IO r) -> IO r
withOutputHandle (Just "-") act = act stdout
withOutputHandle (Just fp) act = withFile fp WriteMode act
withOutputHandle Nothing act = act stdout

getInputHandle :: Maybe FilePath -> IO Handle
getInputHandle (Just "-") = return stdin
getInputHandle (Just fp) = openFile fp ReadMode
getInputHandle Nothing = return stdin

withInputHandle :: Maybe FilePath -> (Handle -> IO r) -> IO r
withInputHandle (Just "-") act = act stdin
withInputHandle (Just fp) act = withFile fp ReadMode act
withInputHandle Nothing act = act stdin

exportShelf :: T.Shelf -> DB.Context -> Maybe FilePath -> Exception IO ()
exportShelf shelf db_ctx output_path = checkExists >> doExport
  where
    checkExists = checkShelfExists shelf db_ctx
    doExport = liftIO $ withOutputHandle output_path $ IE.exportToHandle shelf db_ctx

importShelf :: Maybe FilePath -> DB.Context -> Exception IO ()
importShelf fp db_ctx =
  (liftIO $ getInputHandle fp)
    >>= \file -> (IE.importFromHandle file db_ctx :: Exception IO T.Shelf) >> return ()

nameForPath :: FilePath -> IO Text
nameForPath path = pack <$> FP.takeFileName <$> DIR.makeAbsolute path

entryNameIsFree :: Text -> DB.Context -> IO Bool
entryNameIsFree name db_ctx = getEntry >>= \entry -> DB.exists entry db_ctx
  where
    getEntry = DB.makeEntry name "" db_ctx

entryNameFromPathIfUnique :: FilePath -> DB.Context -> IO (Maybe Text)
entryNameFromPathIfUnique path db_ctx = getName >>= checkUnique
  where
    getName = FSM.nameForPath path
    checkUnique name = uniqueOrNothing name <$> FSM.entryNameIsFree name db_ctx
    uniqueOrNothing :: Text -> Bool -> Maybe Text
    uniqueOrNothing name exists =
      if not exists
        then Just name
        else Nothing

connectToDbWithTmpShelf :: Maybe FilePath -> EX.Exception IO DB.Context
connectToDbWithTmpShelf path = liftIO (getInputHandle path) >>= doImport
  where
    doImport handle = IE.importShelfFromHandleRaw handle >>= doConnect

    doConnect :: (T.Shelf, [T.Entry]) -> EX.Exception IO DB.Context
    doConnect raw_shelf@(shelf, entries) =
      connectToDb (Just shelf) (Just ":memory:")
        >>= \db -> liftIO $ insertEntries raw_shelf db >> return db

insertEntries :: (T.Shelf, [T.Entry]) -> DB.Context -> IO ()
insertEntries (shelf, entries) db_ctx = maybeInsertShelf >> DB.insertMany entries db_ctx
  where
    maybeInsertShelf =
      DB.exists shelf db_ctx >>= \exists ->
        if (not exists)
          then DB.insert shelf db_ctx
          else return ()
