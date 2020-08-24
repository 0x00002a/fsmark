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

import Control.Monad.Except (liftIO, runExceptT)
import DB
  ( defaultShelfId,
    defaultShelfName,
    getShelfId,
  )
import qualified DB
import Data.Text (Text)
import qualified Exceptions as EX
import qualified FSM as L
import qualified Frontend as F
import Test.HUnit
import TestsDB (resetDb, testDb)
import qualified Types as T

assertDBItemExists :: (DB.DBObject a) => a -> DB.Context -> IO ()
assertDBItemExists entry ctx = assertBool "Database entry exists" =<< DB.exists entry ctx

assertNotDBItemExists :: (DB.DBObject a) => a -> DB.Context -> IO ()
assertNotDBItemExists entry ctx = (\exists -> assertBool "Database entry does not exist" (not exists)) =<< DB.exists entry ctx

printExcept :: Either EX.Error b -> IO Bool
printExcept either = case either of
  Right _ -> return True
  Left err -> EX.printError err >> return False

assertNoErr :: EX.Exception IO () -> IO ()
assertNoErr except = runExceptT except >>= \either_except -> printExcept either_except >>= assertBool "Check for exception"

createShelf :: Text -> DB.Context -> IO DB.Shelf
createShelf name db = (\shelf -> DB.insert shelf db >> return shelf) (DB.ShelfName name)

createEntry :: Text -> DB.Shelf -> DB.Context -> IO T.Entry
createEntry name shelf db = (\file -> DB.insert file db >> return file) (T.Entry name "" shelf)

addEntryTest = TestCase (testDb >>= (\db -> assertNoErr $ L.addEntry (T.Entry "test1" "." (DB.target_shelf db)) db >> (liftIO $ assertDBItemExists (T.Entry "test1" "" DB.dummyShelf) db)))

addShelfTest = TestCase (testDb >>= (\db -> assertNoErr $ L.addShelf (T.ShelfName "test-shelf") db >> (liftIO $ assertDBItemExists (DB.ShelfName "test-shelf") db)))

getShelfIdTest = TestCase (testDb >>= (\db -> createShelf "idShelfTest" db >>= \shelf -> DB.dbId shelf db >> assertDBItemExists shelf db))

removeDefaultTest = TestCase (testDb >>= \db -> assertNoErr $ F.parseShelvesCmd (F.RemoveShelf "default" True) db >> checkNotRemoved db)
  where
    checkNotRemoved = \db -> liftIO $ assertDBItemExists (DB.defaultShelf) db

addRemoveShelfTest = TestCase (testDb >>= (\db -> assertNoErr $ createShelf db >> rmShelf db >> doAssert db))
  where
    createShelf db = L.addShelf (T.ShelfName "test-shelf2") db
    rmShelf db = F.parseShelvesCmd (F.RemoveShelf "test-shelf2" True) db
    doAssert db = liftIO $ assertNotDBItemExists (DB.ShelfName "test-shelf2") db

dbCopyEntryTest = TestCase (testDb >>= (\db -> createEntry "copyDbTest" DB.defaultShelf db >>= \entry -> createShelf "copyToDbShelf" db >>= \shelf -> DB.copyEntryTo shelf (DB.name entry) db >> assertDBItemExists entry db))

copyEntryTest = TestCase (testDb >>= (\db -> createEntry "copyTest" DB.defaultShelf db >>= \entry -> createShelf "copyToShelf" db >>= \shelf -> setupAndTest entry shelf db))
  where
    setupAndTest = \entry shelf db -> doCopy (DB.name entry) shelf db >> (liftIO $ assertDBItemExists entry db)
    doCopy = \entry_name to_shelf db -> assertNoErr $ L.copyEntry DB.defaultShelf to_shelf entry_name db

moveEntryTest = TestCase (testDb >>= (\db -> createEntry "moveTest" DB.defaultShelf db >>= \entry -> createShelf "moveToShelf" db >>= \shelf -> setupAndTest entry shelf db))
  where
    setupAndTest = \entry shelf db -> doMove entry shelf db >> (liftIO $ assertNotDBItemExists entry db)
    doMove = \entry (DB.ShelfName to_name) db -> assertNoErr $ L.moveEntryByName DB.defaultShelfName to_name (DB.name entry) db

changeTargetShelfTest = TestCase (testDb >>= (\db -> createShelf "targetShelfTester" db >>= \shelf -> (\new_db -> assertEqual "Target and change to shelf" (DB.target_shelf new_db) shelf) (DB.changeTargetShelf shelf db)))

renameEntryTest = TestCase (testDb >>= (\db -> createEntry "renameEntryTest" DB.defaultShelf db >>= \entry -> doRename entry db >> (liftIO $ assertDBItemExists (renamedFile entry) db)))
  where
    renamedFile = \file -> file {DB.name = "renamedEntry"}
    doRename entry db = assertNoErr $ L.renameEntryByName (DB.name entry) "renamedEntry" db

renameShelfTest =
  TestCase
    ( testDb
        >>= ( \db ->
                createShelf "renameShelf" db
                  >>= \shelf ->
                    createEntry "shelfRenameEntry" shelf db
                      >>= \entry ->
                        assertNoErr $
                          L.renameShelfByName "renameShelf" "renamedShelf" db
                            >> (liftIO $ assertShelfExists shelf db)
                            >> (liftIO $ assertDBItemExists entry db)
            )
    )
  where
    renamedShelf = DB.ShelfName "renamedShelf"
    assertShelfExists = \shelf db -> DB.exists renamedShelf db >>= \does_exist -> assertBool "Shelf exists" does_exist

createEntryOnShelfTest = TestCase (testDb >>= (\db -> createShelf "createEntryShelfTest" db >>= \shelf -> createEntry "createEntryTest" shelf db >>= \entry -> assertDBItemExists entry db >> assertDBItemExists shelf db))

tests =
  TestList
    [ TestLabel "Add entry" addEntryTest,
      TestLabel "Add shelf" addShelfTest,
      TestLabel "Remove default prevention" removeDefaultTest,
      TestLabel "Add and remove a shelf" addRemoveShelfTest,
      TestLabel "Get shelve id by name" getShelfIdTest,
      TestLabel "Change database context target shelf" changeTargetShelfTest,
      TestLabel "Copy entry with underlying database functions" dbCopyEntryTest,
      TestLabel "Copy entry" copyEntryTest,
      TestLabel "Move entry" moveEntryTest,
      TestLabel "Rename entry" renameEntryTest,
      TestLabel "Rename Shelf" renameShelfTest,
      TestLabel "Create shelf and add entry" createEntryOnShelfTest
    ]

main :: IO Counts
main = resetDb >> runTestTT tests
