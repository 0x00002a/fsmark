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
                                                , fromMaybe, isJust
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
import qualified Sys
import Data.List (find, filter)


addEntry :: T.Entry -> T.Shelf -> T.Shelf
addEntry entry shelf = shelf { T.s_entries = (T.s_entries shelf ++ [entry]) }

copyEntryByName' :: Text -> Text -> Text -> IO T.Shelf
copyEntryByName' from to ent = 
    DB.loadShelfFromName from 
    >>= \fshelf -> DB.loadShelfFromName to 
    >>= \tshelf -> doCopy fshelf tshelf
    where 
        doCopy (Just f) (Just t) = copyEntryByName f t ent
        doCopy Nothing _ = throw $ EX.DoesNotExist $ EX.Shelf from
        doCopy _ Nothing = throw $ EX.DoesNotExist $ EX.Shelf to

copyEntryByName :: T.Shelf -> T.Shelf -> Text -> IO T.Shelf
copyEntryByName from to entry = if from == to
    then throw $ BadInput "Source and destination shelves must be different"
    else existsCheck >> doCopy
  where
    doCopy = doAdd $ find (\e -> T.name e == entry) (T.s_entries from) 
    doAdd (Just ent) = return $ addEntry ent to 
    existsCheck = checkShelfExists (T.s_name from) >> checkShelfExists (T.s_name to) 

entriesMatching :: T.Shelf -> Maybe Text -> [T.Entry] 
entriesMatching shelf Nothing = T.s_entries shelf
entriesMatching shelf (Just name) = filter (entryNameMatches name) $ T.s_entries shelf

checkShelfExists :: Text -> IO ()
checkShelfExists shelf = DB.shelfExists shelf >>= check
    where 
        check False = throw $ EX.DoesNotExist $ EX.Shelf shelf
        check True = return ()

renameShelfByName :: Text -> Text -> IO ()
renameShelfByName from to = 
    checkShelfExists from >> DB.loadShelfFromName from >>= save
    where 
        save (Just shelf) = DB.saveShelfDefault (shelf { T.s_name = to })  

entryNameMatches :: Text -> T.Entry -> Bool
entryNameMatches name ent = T.name ent == name  

entryNameNotMatches :: Text -> T.Entry -> Bool
entryNameNotMatches t e = not $ entryNameMatches t e

removeEntryByName :: T.Shelf -> Text -> T.Shelf 
removeEntryByName from entry = from { T.s_entries = filter (entryNameNotMatches entry) (T.s_entries from) }

removeShelfByName :: Text -> IO ()
removeShelfByName name = DB.removeShelf name

renameEntryByName :: Text -> Text -> T.Shelf -> T.Shelf
renameEntryByName from to shelf = shelf { T.s_entries = map modEl (T.s_entries shelf) }
    where 
        modEl el = if (entryNameMatches from el) 
            then el { T.name = to }
            else el

copyEntry :: T.Shelf -> T.Shelf -> T.Entry -> IO T.Shelf
copyEntry from to ent = copyEntryByName from to (T.name ent)

moveEntry :: T.Shelf -> T.Shelf -> T.Entry -> IO ()
moveEntry from to ent =
    copyEntry from to ent 
        *> DB.saveShelfDefault (removeEntryByName from (T.name ent))

checkEntryExistsByName :: Text -> T.Shelf -> Bool
checkEntryExistsByName entry_name shelf = isJust $ find (\e -> (T.name e) == entry_name) (T.s_entries shelf) 

checkEntryExists :: T.Entry -> T.Shelf -> Bool
checkEntryExists entry_name shelf = isJust $ find ((==) entry_name) (T.s_entries shelf) 

checkFileNotExists :: T.Entry -> T.Shelf -> IO ()
checkFileNotExists entry shelf = check $ find ((==) entry) (T.s_entries shelf) 
    where 
        check Nothing = return ()
        check (Just _) = throw $ EX.NamingConflict $ EX.Entry (T.name entry)

checkShelfNotExists :: Text -> IO ()
checkShelfNotExists shelf = DB.shelfExists shelf >>= \exists ->
    when exists $ throw $ EX.NamingConflict $ EX.Shelf shelf

addShelf :: Text -> IO ()
addShelf shelf = checkShelfNotExists shelf >> DB.saveShelfDefault (T.Shelf shelf [])

moveEntryByName :: Text -> Text -> Text -> IO T.Shelf
moveEntryByName from to name = 
    DB.loadShelfFromName from >>= \fshelf -> DB.loadShelfFromName to >>= \tshelf -> doRemove fshelf tshelf
    where 
        doRemove :: Maybe T.Shelf -> Maybe T.Shelf -> IO T.Shelf
        doRemove (Just f) (Just t) = (\s -> removeEntryByName s name) <$> copyEntryByName f t name 

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

exportShelf :: Maybe FilePath -> T.Shelf -> IO ()
exportShelf path shelf = checkExists >> doExport
  where
    checkExists = checkShelfExists (T.s_name shelf)
    doExport    = withOutputHandle path $ IE.exportToHandle shelf 

importShelf :: Maybe FilePath -> IO (Maybe T.Shelf)
importShelf fp = getInputHandle fp >>= IE.importFromHandle

nameForPath :: FilePath -> IO Text
nameForPath path = pack . FP.takeFileName <$> DIR.makeAbsolute path


entryFromPath :: FilePath -> IO T.Entry
entryFromPath path =
    T.Entry <$> nameForPath path <*> (pack <$> DIR.makeAbsolute path)

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

makeEntry :: Text -> FilePath -> IO T.Entry
makeEntry name fp =
    T.Entry name . pack <$> (Sys.expandPath fp >>= DIR.makeAbsolute)

type ItemExistsMap a = [(a, Bool)]

notExistsOutOf :: [(a, Bool)] -> [a]
notExistsOutOf items = [ fst it | it <- items, not $ snd it ]

existsOutOf :: [(a, Bool)] -> [a]
existsOutOf items = [ fst it | it <- items, snd it ]

checkSearchable :: FilePath -> IO Bool
checkSearchable fp = DIR.searchable <$> DIR.getPermissions fp

extractDuplicates :: (Eq a, Ord a) => [a] -> ([a], [a])
extractDuplicates items = (DU.unique items, DU.repeated items)
