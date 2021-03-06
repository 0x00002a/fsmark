-- Copyright (C) 2020 Natasha England-Elbro
--
-- This file is part of file-shelf.
--
-- file-shelf is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- file-shelf is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
-- A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along with
-- file-shelf.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE OverloadedStrings #-}

module DB
    ( connect
    , getFiles
    , getAllFiles
    , removeFile
    , DBObject
        ( insert
        , remove
        , retrieveAll
        , exists
        , dbId
        , rename
        , retrieveAllLike
        , getName
        )
    , Entry(path, name)
    , Shelf(ShelfName)
    , Context(..)
    , ConnectionString(..)
    , ConnectionType(..)
    , makeEntry
    , defaultShelfName
    , removeShelf
    , getAllShelfNames
    , changeTargetShelf
    , copyEntryTo
    , wipeDb
    , getShelfId
    , defaultShelfId
    , dummyShelf
    , defaultShelf
    , copyEntry
    , insertMany
    , DBAction
    )
where

import           Control.Exception              ( throw )
import           Control.Monad                  ( forM
                                                , (>=>)
                                                , unless
                                                , when
                                                )
import           Control.Monad.Cont             ( ContT(..) )
import           Control.Monad.Except           ( liftIO
                                                , throwError
                                                )
import           Data.Text                      ( Text
                                                , append
                                                , pack
                                                , unpack
                                                )
import qualified Data.Text                     as T
import           Database.SQLite.Simple         ( Only(..)
                                                , query_
                                                )
import qualified Database.SQLite.Simple        as SQL
import qualified Exceptions                    as EX
import qualified Pretty
import qualified System.Directory              as DIR
import           System.FilePath                ( (</>) )
import           Text.Printf                    ( printf )
import           Types
import qualified Types                         as T

class DBObject a where
  exists :: a -> Context -> DBAction Bool
  insert :: a -> Context -> DBAction ()
  dbId :: a -> Context -> DBAction Integer
  remove :: a -> Context -> DBAction ()
  retrieveAll :: Context -> DBAction [a]
  rename :: a -> Text -> Context -> DBAction ()
  getName :: a -> Context -> DBAction Text
  retrieveAllLike :: Text -> Context -> DBAction [a]
  insertMany :: [a] -> Context -> DBAction ()
  insertMany entries ctx = SQL.withTransaction (conn ctx) doInsert
    where
      doInsert = mapM_ (`insert` ctx) entries

data Context = Context
  { conn :: SQL.Connection,
    target_shelf :: Shelf,
    doing_dryrun :: Bool
  }

data ConnectionString = Path String | InMemory | DefaultDB

data ConnectionType = Normal | DryRun

type DBAction a = IO a --ContT () IO a

type InternalShelfID = Integer

execute :: (SQL.ToRow q) => Context -> SQL.Query -> q -> DBAction ()
execute ctx query args =
    unless (doing_dryrun ctx) $ SQL.execute (conn ctx) query args

query
    :: (SQL.ToRow q, SQL.FromRow r) => Context -> SQL.Query -> q -> DBAction [r]
query ctx = SQL.query (conn ctx)

instance SQL.FromRow T.Entry where
    fromRow = T.Entry <$> SQL.field <*> SQL.field

instance DBObject Shelf where
    insert (ShelfName name) ctx =
        execute ctx "INSERT INTO shelves (name) VALUES (?)" (Only name)
    dbId (ShelfID   id  ) _   = return id
    dbId (ShelfName name) ctx = getShelfId name ctx
    remove (ShelfID id) ctx =
        getShelfName id ctx >>= \name -> removeShelf name ctx
    remove (ShelfName name) ctx = removeShelf name ctx
    retrieveAll ctx = map ShelfName <$> getAllShelfNames ctx
    exists (ShelfName name) ctx = nestedNth 0 <$> query
        ctx
        "SELECT EXISTS (SELECT 1 FROM shelves WHERE name = ?)"
        (Only name)
    exists (ShelfID id) ctx = nestedNth 0 <$> query
        ctx
        "SELECT EXISTS (SELECT 1 FROM shelves WHERE id = ?)"
        (Only id)
    rename (ShelfName name) to ctx =
        execute ctx "UPDATE shelves SET name = ? WHERE name = ?" (to, name)
    rename (ShelfID id) to ctx =
        execute ctx "UPDATE shelves SET name = ? WHERE id = ?" (to, id)
    getName (ShelfName name) _   = return name
    getName (ShelfID   id  ) ctx = getShelfName id ctx
    retrieveAllLike name ctx = map ShelfName . setSecondD <$> query
        ctx
        "SELECT name FROM shelves WHERE name LIKE ?"
        (Only (formatLikeExpr name))

instance DBObject Entry where
    insert file ctx = execQuery =<< targetShelfId ctx
      where
        execQuery =
            execute
                    ctx
                    "INSERT INTO files (name, path, shelf_id) VALUES (?, ?, ?)"
                . args
        args id = (name file, path file, id)

    dbId file ctx = nestedNth 0 <$> (execQuery =<< targetShelfId ctx)
      where
        execQuery id = query
            ctx
            "SELECT id FROM files WHERE name = ? AND shelf_id = ?"
            (name file, id)
    remove file = removeFile (name file)
    retrieveAll = getAllFiles
    exists file ctx =
        nestedNth 0
            <$> (   (\id -> query
                        ctx
                        "SELECT EXISTS (SELECT 1 FROM files WHERE name = ? AND shelf_id = ?)"
                        (name file, id)
                    )
                =<< targetShelfId ctx
                )
    rename file to ctx =
        (\id -> execute
                ctx
                "UPDATE files SET name = ? WHERE name = ? AND shelf_id = ?"
                (to, name file, id)
            )
            =<< targetShelfId ctx
    getName file _ = return $ name file
    retrieveAllLike name ctx = map rsToFile <$> execQuery
      where
        execQuery = targetShelfId ctx >>= \id -> query
            ctx
            "SELECT name, path, shelf_id FROM files WHERE name LIKE ? AND shelf_id = ?"
            (formatLikeExpr name, id)

dbPath :: IO String
dbPath = (</> "data.db") <$> dir
  where
    dir = DIR.getXdgDirectory DIR.XdgData "fsm"
        >>= \p -> DIR.createDirectoryIfMissing True p >> return p

dbTables :: [Text]
dbTables =
    [ "CREATE TABLE IF NOT EXISTS shelves (id INTEGER PRIMARY KEY, name TEXT NOT NULL UNIQUE, is_default BOOLEAN NOT NULL DEFAULT 0);"
    , "CREATE TABLE IF NOT EXISTS files (id INTEGER PRIMARY KEY, name TEXT NOT NULL, path TEXT NOT NULL, shelf_id INTEGER NOT NULL, FOREIGN KEY (shelf_id) REFERENCES shelves(id) ON DELETE CASCADE ON UPDATE CASCADE, UNIQUE (name, shelf_id));"
    , "INSERT OR IGNORE INTO shelves (name, id, is_default) VALUES ('default', 0, 1);"
    ]

initDb :: SQL.Connection -> IO ()
initDb conn = mapM_ (SQL.execute_ conn . SQL.Query) dbTables

connect :: T.Shelf -> ConnectionString -> ConnectionType -> DBAction Context
connect shelf conn_str conn_type =
    getPath
        >>= (   SQL.open
            >=> (\conn -> initAndGetShelfId conn >> createContext conn)
            )
  where
    getPath = case conn_str of
        Path p    -> checkAndEnsureExists p >> return p
        InMemory  -> return ":memory:"
        DefaultDB -> dbPath
    checkAndEnsureExists p = return ()
    initAndGetShelfId = case conn_type of
        Normal -> initDb
        DryRun -> \ctx -> return ()
    createContext conn = optimiseContext $ Context conn shelf isDryRun
    isDryRun = case conn_type of
        DryRun -> True
        _      -> False
    ensureExists p exists =
        if exists then return () else DIR.createDirectoryIfMissing True p

getFiles :: Text -> Context -> DBAction [Entry]
getFiles name context = map handler <$> (res =<< targetShelfId context)
  where
    res id = query
        context
        "SELECT name, path FROM files WHERE name = ? AND shelf_id = ?"
        (name, id)
    handler = uncurry Entry

getAllFiles :: Context -> DBAction [Entry]
getAllFiles context = stmt =<< targetShelfId context
  where
    stmt id = query context
                    "SELECT name, path FROM files WHERE shelf_id = ?"
                    (Only id)

mapToShelves :: [Text] -> [Shelf]
mapToShelves = map ShelfName

removeFile :: Text -> Context -> DBAction ()
removeFile name context = stmt =<< targetShelfId context
  where
    stmt id = execute context
                      "DELETE FROM files WHERE name = ? AND shelf_id = ?"
                      (name, id)

getShelfName :: Integer -> Context -> DBAction Text
getShelfName sid context = head . head <$> query
    context
    "SELECT name FROM shelves WHERE id = ?"
    (Only sid)

getShelfId :: Text -> Context -> DBAction Integer
getShelfId shelf_name context = nestedNth 0 <$> query
    context
    "SELECT id FROM shelves WHERE name = ?"
    (Only shelf_name)

nestedNth :: Int -> [[a]] -> a
nestedNth n as = (!! n) as !! n

defaultShelfId :: Context -> DBAction Integer
defaultShelfId context = nestedNth 0 <$> SQL.query_
    (conn context)
    "SELECT id FROM shelves WHERE is_default = 1"


makeEntry :: Text -> Prelude.FilePath -> Context -> IO Entry
makeEntry name path context = ctor <$> getDir
  where
    getDir = pack <$> DIR.makeAbsolute path
    ctor   = Entry name

defaultShelfName :: Text
defaultShelfName = "default"

getAllShelfNames :: Context -> DBAction [Text]
getAllShelfNames ctx =
    map head <$> SQL.query_ (conn ctx) "SELECT name FROM shelves"

removeShelf :: Text -> Context -> DBAction ()
removeShelf name ctx = execute
    ctx
    "DELETE FROM shelves WHERE name = ? AND is_default = 0"
    (Only name)

changeTargetShelf :: Shelf -> Context -> Context
changeTargetShelf shelf ctx = ctx { target_shelf = shelf }

copyEntryTo :: Shelf -> Text -> Context -> DBAction ()
copyEntryTo to_shelf name ctx =
    (\to_ctx -> getFiles name ctx >>= \files -> insert (head files) to_ctx)
        $ changeTargetShelf to_shelf ctx


copyEntry :: Shelf -> Shelf -> Text -> Context -> DBAction ()
copyEntry from to entry ctx = getEntries
    >>= \entries -> checkUnique entries to_ctx >> insertMany entries to_ctx
  where
    getEntries = retrieveAllLike entry from_ctx :: DBAction [T.Entry]
    from_ctx   = changeTargetShelf from ctx
    to_ctx     = changeTargetShelf to ctx
    checkUnique :: [T.Entry] -> Context -> DBAction ()
    checkUnique entries ctx = checkAllUnique entries ctx >>= \all_unique ->
        when all_unique $ throw $ EX.NamingConflict $ EX.Entry entry

wipeDb :: Context -> DBAction ()
wipeDb ctx =
    unless (doing_dryrun ctx)
        $  SQL.execute_ (conn ctx) "DROP TABLE shelves"
        >> SQL.execute_ (conn ctx) "DROP TABLE files"
        >> initDb (conn ctx)

targetShelfId :: Context -> DBAction Integer
targetShelfId ctx = dbId (target_shelf ctx) ctx

dummyShelf :: Shelf
dummyShelf = ShelfID 0

defaultShelf :: Shelf
defaultShelf = ShelfName "default"

setSecondD :: [[a]] -> [a]
setSecondD = map head

rsToFile :: (Text, Text, Integer) -> Entry
rsToFile (name, path, shelf) = Entry name path

formatLikeExpr :: Text -> Text
formatLikeExpr = T.map repl
  where
    repl '*' = '%'
    repl ch  = ch

checkUnique entry ctx = not <$> DB.exists entry ctx

checkAllUnique :: (DBObject a) => [a] -> Context -> DBAction Bool
checkAllUnique entries ctx =
    mapM (`checkUnique` ctx) entries >>= \all_unique -> return $ and all_unique

optimiseContext :: Context -> DBAction Context
optimiseContext ctx =
    targetShelfId ctx >>= \id -> return ctx { target_shelf = ShelfID id }
