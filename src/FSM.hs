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

import           Control.Exception              ( throw )
import           Control.Monad.Cont
import qualified DB
import qualified Data.List.Unique              as DU
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Data.Text                      ( Text
                                                , append
                                                , pack
                                                , unpack
                                                )
import           Exceptions                     ( Error(BadInput, DoesNotExist)
                                                , Exception
                                                )
import qualified Exceptions                    as EX
import qualified ImportExport                  as IE
import qualified Json                          as J
import qualified System.Directory              as DIR
import           System.FilePath                ( combine )
import qualified System.FilePath               as FP
import           System.IO                      ( FilePath
                                                , Handle
                                                , IOMode(ReadMode, WriteMode)
                                                , hClose
                                                , openFile
                                                , stdin
                                                , stdout
                                                , withFile
                                                )
import qualified Types                         as T

cleanup :: DB.DBAction a -> IO ()
cleanup db_act = return () --runContT db_act (\_ -> return ())

addEntry :: T.Entry -> DB.Context -> DB.DBAction ()
addEntry entry db_ctx =
    checkFileNotExists entry db_ctx >> DB.insert entry db_ctx

copyEntryByName :: T.Shelf -> T.Shelf -> Text -> DB.Context -> DB.DBAction ()
copyEntryByName from to entry db_ctx = if from == to
    then throw $ BadInput "Source and destination shelves must be different"
    else existsCheck
        >> (\src -> doCopy to (DB.changeTargetShelf src db_ctx)) from
  where
    doCopy to src_ctx = DB.copyEntry from to entry db_ctx
    existsCheck = checkShelfExists from db_ctx >> checkShelfExists to db_ctx

getAll :: (DB.DBObject a) => DB.Context -> Maybe Text -> DB.DBAction [a]
getAll db_ctx (Just name_filter) = DB.retrieveAllLike name_filter db_ctx
getAll db_ctx Nothing            = DB.retrieveAll db_ctx

checkShelfExists :: T.Shelf -> DB.Context -> DB.DBAction ()
checkShelfExists shelf db_ctx = DB.exists shelf db_ctx >>= \exists ->
    liftIO $ if exists
        then return ()
        else throw $ EX.DoesNotExist $ EX.Shelf $ getShelfName shelf
  where
    getShelfName (T.ShelfName n) = n
    getShelfName (T.ShelfID   _) = "Unknown"

renameShelfByName :: Text -> Text -> DB.Context -> DB.DBAction ()
renameShelfByName from to db_ctx =
    checkShelfExists shelf db_ctx >> DB.rename shelf to db_ctx
    where shelf = T.ShelfName from

removeEntryByName :: Text -> DB.Context -> DB.DBAction ()
removeEntryByName entry db_ctx =
    checkEntryExistsByName entry db_ctx >> DB.removeFile entry db_ctx

removeItem :: (DB.DBObject a) => a -> DB.Context -> DB.DBAction ()
removeItem item db_ctx = checkExists item db_ctx >> DB.remove item db_ctx

removeShelfByName :: Text -> DB.Context -> DB.DBAction ()
removeShelfByName name = removeItem (T.ShelfName name)

renameEntryByName :: Text -> Text -> DB.Context -> DB.DBAction ()
renameEntryByName from to db_ctx =
    checkEntryExistsByName from db_ctx >> DB.getFiles from db_ctx >>= \files ->
        DB.rename (head files) to db_ctx

checkExists :: (DB.DBObject a) => a -> DB.Context -> DB.DBAction ()
checkExists item db_ctx = DB.exists item db_ctx >>= doCheck
  where
    doCheck does_exist = if does_exist
        then return ()
        else DB.getName item db_ctx >>= \name ->
            liftIO $ throw $ EX.TextError $ name `append` " does not exist"

copyEntry :: T.Shelf -> T.Shelf -> T.Entry -> DB.Context -> IO ()
copyEntry from to ent = copyEntryByName from to (T.name ent)

moveEntry :: T.Shelf -> T.Shelf -> T.Entry -> DB.Context -> IO ()
moveEntry from to ent db_ctx =
    copyEntry from to ent db_ctx
        >> removeEntryByName (T.name ent) (DB.changeTargetShelf from db_ctx)

checkEntryExistsByName :: Text -> DB.Context -> DB.DBAction ()
checkEntryExistsByName entry_name ctx = liftIO (DB.makeEntry entry_name "" ctx)
    >>= \entry -> checkEntryExists entry ctx

checkEntryExists :: T.Entry -> DB.Context -> DB.DBAction ()
checkEntryExists entry ctx = DB.exists entry ctx >>= \exists ->
    liftIO $ if exists
        then return ()
        else throw $ DoesNotExist $ EX.Entry $ T.name entry

checkFileNotExists :: T.Entry -> DB.Context -> DB.DBAction ()
checkFileNotExists entry ctx = DB.exists entry ctx >>= \exists ->
    when exists $ throw $ EX.NamingConflict $ EX.Entry (T.name entry)

checkShelfNotExists :: T.Shelf -> DB.Context -> DB.DBAction ()
checkShelfNotExists shelf ctx = DB.exists shelf ctx >>= \exists ->
    when exists $ throw $ EX.NamingConflict $ EX.Shelf $ shelfDecider shelf
  where
    shelfDecider (T.ShelfName n) = n
    shelfDecider (T.ShelfID   _) = "Unknown"

addShelf :: T.Shelf -> DB.Context -> DB.DBAction ()
addShelf shelf db_ctx =
    checkShelfNotExists shelf db_ctx >> DB.insert shelf db_ctx

connectToDb :: Maybe T.Shelf -> Maybe Text -> Bool -> DB.DBAction DB.Context
connectToDb shelf_name db_path is_dryrun = DB.connect shelf connString connType
  where
    shelf      = fromMaybe DB.defaultShelf shelf_name
    connString = makeConnString db_path
    connType   = makeConnType is_dryrun

makeConnString :: Maybe Text -> DB.ConnectionString
makeConnString (Just ":memory:") = DB.InMemory
makeConnString (Just path      ) = DB.Path $ unpack path
makeConnString Nothing           = DB.DefaultDB

makeConnType True  = DB.DryRun
makeConnType False = DB.Normal

moveEntryByName :: Text -> Text -> Text -> DB.Context -> DB.DBAction ()
moveEntryByName from to name ctx =
    copyEntryByName (T.ShelfName from) (T.ShelfName to) name ctx
        >> removeEntryByName name ctx

withOutputHandle :: Maybe FilePath -> (Handle -> IO r) -> IO r
withOutputHandle (Just "-") act = act stdout
withOutputHandle (Just fp ) act = withFile fp WriteMode act
withOutputHandle Nothing    act = act stdout

getInputHandle :: Maybe FilePath -> IO Handle
getInputHandle (Just "-") = return stdin
getInputHandle (Just fp ) = openFile fp ReadMode
getInputHandle Nothing    = return stdin

withInputHandle :: Maybe FilePath -> (Handle -> IO r) -> IO r
withInputHandle (Just "-") act = act stdin
withInputHandle (Just fp ) act = withFile fp ReadMode act
withInputHandle Nothing    act = act stdin

exportShelf :: T.Shelf -> DB.Context -> Maybe FilePath -> DB.DBAction ()
exportShelf shelf db_ctx output_path = checkExists >> doExport
  where
    checkExists = checkShelfExists shelf db_ctx
    doExport    = withOutputHandle output_path $ IE.exportToHandle shelf db_ctx

importShelf :: Maybe FilePath -> DB.Context -> DB.DBAction ()
importShelf fp db_ctx = getInputHandle fp >>= \file ->
    void (IE.importFromHandle file db_ctx :: DB.DBAction T.Shelf)

nameForPath :: FilePath -> IO Text
nameForPath path = pack . FP.takeFileName <$> DIR.makeAbsolute path

entryNameIsFree :: Text -> DB.Context -> DB.DBAction Bool
entryNameIsFree name db_ctx = liftIO getEntry
    >>= \entry -> DB.exists entry db_ctx
    where getEntry = DB.makeEntry name "" db_ctx

entryNameFromPathIfUnique :: FilePath -> DB.Context -> DB.DBAction (Maybe Text)
entryNameFromPathIfUnique path db_ctx = getName >>= checkUnique
  where
    getName = liftIO $ FSM.nameForPath path
    checkUnique name = uniqueOrNothing name <$> FSM.entryNameIsFree name db_ctx
    uniqueOrNothing :: Text -> Bool -> Maybe Text
    uniqueOrNothing name exists = if not exists then Just name else Nothing


insertEntries :: (T.Shelf, [T.Entry]) -> DB.Context -> DB.DBAction ()
insertEntries (shelf, entries) db_ctx =
    maybeInsertShelf >> DB.insertMany entries db_ctx
  where
    maybeInsertShelf = DB.exists shelf db_ctx
        >>= \exists -> unless exists $ DB.insert shelf db_ctx

entryFromPath :: FilePath -> IO T.Entry
entryFromPath path =
    T.Entry <$> nameForPath path <*> (pack <$> DIR.makeAbsolute path)

createEntriesRecursive :: FilePath -> Integer -> IO [T.Entry]
createEntriesRecursive fp depth =
    listDirRecursive fp depth >>= \paths -> mapM entryFromPath paths

addEntriesRecursive :: FilePath -> Integer -> DB.Context -> DB.DBAction ()
addEntriesRecursive fp depth db_ctx = liftIO (createEntriesRecursive fp depth)
    >>= \entries -> DB.insertMany entries db_ctx

dirExistsOrError :: FilePath -> IO ()
dirExistsOrError dir = DIR.doesDirectoryExist dir >>= \exists -> if exists
    then return ()
    else throw $ EX.TextError $ pack dir `append` " does not exist"

listDir :: FilePath -> IO [FilePath]
listDir dir = checkSearchable dir >>= \ok -> if ok
    then DIR.listDirectory dir
        >>= \paths -> mapM (DIR.canonicalizePath . combine dir) paths
    else return []

listDirRecursive :: FilePath -> Integer -> IO [FilePath]
listDirRecursive _ 0 = return []
listDirRecursive fp depth
    | depth < 0
    = throw $ BadInput "Depth must be non-negative"
    | otherwise
    = DIR.makeAbsolute fp
        >>= (listDir >=> \paths ->
                filterDirs paths >>= exploreNextLevel >>= \next_paths ->
                    return $ next_paths ++ paths
            )

  where
    filterDirs = filterM DIR.doesDirectoryExist
    exploreNextLevel paths =
        concat <$> mapM (\p -> listDirRecursive p (depth - 1)) paths

addMany :: (DB.DBObject a) => [a] -> DB.Context -> DB.DBAction ()
addMany = DB.insertMany

getUniqueOutOf :: (DB.DBObject a) => [a] -> DB.Context -> DB.DBAction [a]
getUniqueOutOf items db_ctx = existenceList
    where existenceList = filterM (`DB.exists` db_ctx) items

makeEntry :: Text -> FilePath -> IO T.Entry
makeEntry name fp = T.Entry name . pack <$> DIR.makeAbsolute fp

type ItemExistsMap a = [(a, Bool)]

generateMapExists
    :: (DB.DBObject a) => DB.Context -> [a] -> DB.DBAction (ItemExistsMap a)
generateMapExists db_ctx items = makeMap <$> generateExistsList
  where
    generateExistsList = mapM (`DB.exists` db_ctx) items
    makeMap            = zip items

notExistsOutOf :: [(a, Bool)] -> [a]
notExistsOutOf items = [ fst it | it <- items, not $ snd it ]

existsOutOf :: [(a, Bool)] -> [a]
existsOutOf items = [ fst it | it <- items, snd it ]

checkSearchable :: FilePath -> IO Bool
checkSearchable fp = DIR.searchable <$> DIR.getPermissions fp

extractDuplicates :: (Eq a, Ord a) => [a] -> ([a], [a])
extractDuplicates items = (DU.unique items, DU.repeated items)
