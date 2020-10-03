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

import           Control.Monad.Except           ( liftIO
                                                , runExceptT
                                                )
import           DB                             ( defaultShelfId
                                                , defaultShelfName
                                                , getShelfId
                                                )
import qualified Sys
import qualified DB
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Exceptions                    as EX
import qualified FSM
import qualified Frontend                      as F
import qualified System.Directory              as DIR
import           Test.HUnit
import           TestsDB                        ( resetDb
                                                , testDb
                                                )
import qualified Types                         as T
import qualified System.Directory              as DIR

assertDBItemExists :: (DB.DBObject a) => a -> DB.Context -> IO ()
assertDBItemExists entry ctx =
    assertBool "Database entry exists" =<< DB.exists entry ctx

assertNotDBItemExists :: (DB.DBObject a) => a -> DB.Context -> IO ()
assertNotDBItemExists entry ctx =
    (\exists -> assertBool "Database entry does not exist" (not exists))
        =<< DB.exists entry ctx

printExcept :: Either EX.Error b -> IO Bool
printExcept either = case either of
    Right _   -> return True
    Left  err -> EX.printError err >> return False

assertNoErr :: EX.Exception IO () -> IO ()
assertNoErr except = runExceptT except >>= \either_except ->
    printExcept either_except >>= assertBool "Check for exception"

createShelf :: Text -> DB.Context -> IO DB.Shelf
createShelf name db =
    (\shelf -> DB.insert shelf db >> return shelf) (DB.ShelfName name)

createEntry :: Text -> DB.Context -> IO T.Entry
createEntry name db =
    (\entry -> DB.insert entry db >> return entry) $ T.Entry name ""

createEntryTest = TestCase (assertPath >> assertName)
  where
    makeEntry  = FSM.makeEntry "test" "."
    correctDir = pack <$> DIR.makeAbsolute "."
    assertPath = correctDir >>= \correct ->
        makeEntry >>= \entry ->
            assertEqual "Entry path correct" correct (T.path entry)
    assertName = makeEntry
        >>= \entry -> assertEqual "Entry name" "test" (T.name entry)

addEntryTest = TestCase
    (   testDb
    >>= (\db ->
            makeEntry >>= \entry ->
                FSM.addEntry entry db >> assertDBItemExists entry db
        )
    )
    where makeEntry = FSM.makeEntry "test1" "."

addShelfTest = TestCase
    (testDb >>= (\db -> FSM.addShelf shelf db >> assertDBItemExists shelf db))
    where shelf = T.ShelfName "test-shelf"

getShelfIdTest = TestCase
    (   testDb
    >>= (\db -> createShelf "idShelfTest" db >>= \shelf ->
            DB.dbId shelf db >> assertDBItemExists shelf db
        )
    )

removeDefaultTest = TestCase
    (testDb >>= \db -> FSM.removeShelfByName "default" db >> checkNotRemoved db)
    where checkNotRemoved = assertDBItemExists (DB.defaultShelf)

addRemoveShelfTest = TestCase
    (testDb >>= (\db -> createShelf db >> rmShelf db >> doAssert db))
  where
    createShelf = FSM.addShelf (T.ShelfName "test-shelf2")
    rmShelf     = FSM.removeShelfByName "test-shelf2"
    doAssert    = assertNotDBItemExists (DB.ShelfName "test-shelf2")

copyEntryTest = TestCase
    (   testDb
    >>= (\db ->
            createEntry "copyTest" db
                >>= \entry -> createShelf "copyToShelf" db
                        >>= \shelf -> setupAndTest entry shelf db
        )
    )
  where
    setupAndTest entry shelf db =
        doCopy entry shelf db >> assertDBItemExists entry db
    doCopy entry to_shelf = FSM.copyEntry DB.defaultShelf to_shelf entry

moveEntryTest = TestCase
    (   testDb
    >>= (\db ->
            createEntry "moveTest" db
                >>= \entry -> createShelf "moveToShelf" db
                        >>= \shelf -> setupAndTest entry shelf db
        )
    )
  where
    setupAndTest entry shelf db =
        doMove entry shelf db >> assertNotDBItemExists entry db
    doMove entry to_name db = FSM.moveEntry DB.defaultShelf to_name entry db

changeTargetShelfTest = TestCase
    (assertChanged $ DB.changeTargetShelf destShelf $ makeCtx)
  where
    assertChanged db =
        assertEqual "Src and dest shelves" destShelf $ DB.target_shelf db
    destShelf = T.ShelfName "changeShelfTester"
    makeCtx   = DB.Context undefined DB.defaultShelf False

renameEntryTest = TestCase
    (   testDb
    >>= (\db ->
            createEntry "renameEntryTest" db
                >>= \entry ->
                        doRename entry db
                            >> assertDBItemExists (renamedFile entry) db
        )
    )
  where
    renamedFile = \file -> file { DB.name = "renamedEntry" }
    doRename entry db = FSM.renameEntryByName (DB.name entry) "renamedEntry" db

renameShelfTest = TestCase
    (   testDb
    >>= (\db -> createShelf "renameShelf" db >>= \shelf ->
            createEntry "shelfRenameEntry" db >>= \entry ->
                FSM.renameShelfByName "renameShelf" "renamedShelf" db
                    >> assertShelfExists shelf db
                    >> assertDBItemExists entry db
        )
    )
  where
    renamedShelf      = DB.ShelfName "renamedShelf"
    assertShelfExists = \shelf db -> DB.exists renamedShelf db
        >>= \does_exist -> assertBool "Shelf exists" does_exist

createEntryOnShelfTest = TestCase
    (   testDb
    >>= (\db -> createShelf "createEntryShelfTest" db >>= \shelf ->
            createEntry "createEntryTest" db
                >>= \entry ->
                        assertDBItemExists entry db
                            >> assertDBItemExists shelf db
        )
    )

importExportTest = TestCase
    (testDb >>= \db -> createShelf "exportedShelf" db >>= \shelf ->
        createEntry "exportedEntry" db >>= \entry ->
            FSM.exportShelf shelf db (Just "test-out.json")
                >> FSM.removeItem shelf db
                >> FSM.importShelf (Just "test-out.json") db
                >> assertDBItemExists shelf db
                >> assertDBItemExists entry db
    )

escapePathsTest = TestCase $ winTest >> unixTest >> unknownTest
  where
    winTest = assertEqual "Windows path escaped"
                          "/home^ /test"
                          (Sys.escapePath "/home /test" Sys.Windows)
    unixTest = assertEqual "Unix path escaped"
                           "/home\\ /test"
                           (Sys.escapePath "/home /test" Sys.Unix)
    unknownTest = assertEqual "Unknown system path escaped"
                              "/home\" \"/test"
                              (Sys.escapePath "/home /test" Sys.Unknown)


osFromStrTest = TestCase $ winTest >> linuxTest >> macTest >> unknownTest
  where
    winTest = assertEqual "Windows OS is correct" Sys.Windows
        $ Sys.osFromStr "mingw32"
    linuxTest =
        assertEqual "Linux OS is correct" Sys.Unix $ Sys.osFromStr "linux"
    macTest = assertEqual "Mac OS is correct" Sys.Unix $ Sys.osFromStr "darwin"
    unknownTest = assertEqual "Unknown OS is detected" Sys.Unknown
        $ Sys.osFromStr "obscureOsName"

expandPathTest = TestCase $ homeDir >>= \home ->
    Sys.expandPath "~/file.txt" >>= \expanded ->
        assertEqual "Path expands properly" (home ++ "/file.txt") expanded
    where homeDir = DIR.getHomeDirectory

tests = TestList
    [ TestLabel "Add entry"                            addEntryTest
    , TestLabel "Add shelf"                            addShelfTest
    , TestLabel "Remove default prevention"            removeDefaultTest
    , TestLabel "Add and remove a shelf"               addRemoveShelfTest
    , TestLabel "Get shelve id by name"                getShelfIdTest
    , TestLabel "Change database context target shelf" changeTargetShelfTest
    , TestLabel "Copy entry"                           copyEntryTest
    , TestLabel "Move entry"                           moveEntryTest
    , TestLabel "Rename entry"                         renameEntryTest
    , TestLabel "Rename Shelf"                         renameShelfTest
    , TestLabel "Create shelf and add entry"           createEntryOnShelfTest
    , TestLabel "Import and export a shelf"            importExportTest
    , TestLabel "Create entry"                         createEntryTest
    , TestLabel "Escaping paths works"                 escapePathsTest
    , TestLabel "Reading OS from string works"         osFromStrTest
    , TestLabel "Expanding paths works"                expandPathTest
    ]

main :: IO Counts
main = resetDb >> runTestTT tests
