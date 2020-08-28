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

module DB
  ( connect,
    getFiles,
    getAllFiles,
    removeFile,
    DBObject (insert, remove, retrieveAll, exists, dbId, rename, retrieveAllLike, getName),
    Entry (path, name),
    Shelf (ShelfName),
    Context (..),
    ConnectionString (..),
    ConnectionType (..),
    makeEntry,
    defaultShelfName,
    removeShelf,
    getAllShelfNames,
    changeTargetShelf,
    copyEntryTo,
    wipeDb,
    getShelfId,
    defaultShelfId,
    dummyShelf,
    defaultShelf,
    copyEntry,
    insertMany,
    DBAction,
  )
where

import Control.Exception (throw)
import Control.Monad (forM)
import Control.Monad.Cont (ContT (..))
import Control.Monad.Except (liftIO, throwError)
import Data.Text (Text, append, pack, unpack)
import qualified Data.Text as T
import Database.SQLite.Simple
  ( Only (..),
    query_,
  )
import qualified Database.SQLite.Simple as SQL
import qualified Exceptions as EX
import qualified Pretty
import qualified System.Directory as DIR
import System.FilePath ((</>))
import Text.Printf (printf)
import Types
import qualified Types as T

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
  insertMany entries ctx = beginTransaction ctx >>= \t -> doInsert >> endTransaction t
    where
      doInsert = mapM_ (\ent -> insert ent ctx) entries

data Context = Context
  { conn :: DBAction SQL.Connection,
    target_shelf :: Shelf,
    doing_dryrun :: Bool
  }

data ConnectionString = Path String | InMemory | DefaultDB

data ConnectionType = Normal | DryRun

type DBAction a = ContT () IO a

beginTransaction ctx = conn ctx >>= \c -> liftIO $ SQL.execute_ c "BEGIN TRANSACTION" >> return ctx

endTransaction ctx = conn ctx >>= \c -> liftIO $ SQL.execute_ c "END TRANSACTION"

execute :: (SQL.ToRow q) => Context -> SQL.Query -> q -> DBAction ()
execute ctx query args
  | (doing_dryrun ctx) = return ()
  | otherwise = (conn ctx) >>= \c -> liftIO $ SQL.execute c query args

query :: (SQL.ToRow q, SQL.FromRow r) => Context -> SQL.Query -> q -> DBAction [r]
query ctx query args = conn ctx >>= \c -> liftIO $ SQL.query c query args

instance DBObject Shelf where
  insert (ShelfName name) ctx = execute ctx "INSERT INTO shelves (name) VALUES (?)" (Only name)
  dbId (ShelfID id) _ = return id
  dbId (ShelfName name) ctx = getShelfId name ctx
  remove (ShelfID id) ctx = getShelfName id ctx >>= \name -> removeShelf name ctx
  remove (ShelfName name) ctx = removeShelf name ctx
  retrieveAll ctx = (\names -> map (\name -> ShelfName name) names) <$> getAllShelfNames ctx
  exists (ShelfName name) ctx = nestedNth 0 <$> query ctx "SELECT EXISTS (SELECT 1 FROM shelves WHERE name = ?)" (Only name)
  exists (ShelfID id) ctx = nestedNth 0 <$> query ctx "SELECT EXISTS (SELECT 1 FROM shelves WHERE id = ?)" (Only id)
  rename (ShelfName name) to ctx = execute ctx "UPDATE shelves SET name = ? WHERE name = ?" (to, name)
  rename (ShelfID id) to ctx = execute ctx "UPDATE shelves SET name = ? WHERE id = ?" (to, id)
  getName (ShelfName name) _ = return name
  getName (ShelfID id) ctx = getShelfName id ctx
  retrieveAllLike name ctx = (\names -> map (\name -> ShelfName name) (setSecondD names)) <$> query ctx "SELECT name FROM shelves WHERE name LIKE ?" (Only (formatLikeExpr name))

instance DBObject Entry where
  insert file ctx = execQuery =<< targetShelfId ctx
    where
      execQuery = \id -> execute ctx "INSERT INTO files (name, path, shelf_id) VALUES (?, ?, ?)" $ args id
      args = \id -> ((name file), (path file), id)

  dbId file ctx = nestedNth 0 <$> (execQuery =<< targetShelfId ctx)
    where
      execQuery = \id -> query ctx "SELECT id FROM files WHERE name = ? AND shelf_id = ?" ((name file), id)
  remove file ctx = removeFile (name file) ctx
  retrieveAll ctx = getAllFiles ctx
  exists file ctx = nestedNth 0 <$> ((\id -> query ctx "SELECT EXISTS (SELECT 1 FROM files WHERE name = ? AND shelf_id = ?)" ((name file), id)) =<< targetShelfId ctx)
  rename file to ctx = (\id -> execute ctx "UPDATE files SET name = ? WHERE name = ? AND shelf_id = ?" (to, (name file), id)) =<< dbId (shelf_id file) ctx
  getName file _ = return $ name file
  retrieveAllLike name ctx = (\res -> map rsToFile res) <$> execQuery
    where
      execQuery = targetShelfId ctx >>= \id -> query ctx "SELECT name, path, shelf_id FROM files WHERE name LIKE ? AND shelf_id = ?" (formatLikeExpr name, id)

dbPath :: IO String
dbPath = (\p -> p </> "data.db") <$> dir
  where
    dir = DIR.getXdgDirectory DIR.XdgData "fsm" >>= (\p -> (\() -> p) <$> DIR.createDirectoryIfMissing True p)

dbTables :: [Text]
dbTables =
  [ "CREATE TABLE IF NOT EXISTS shelves (id INTEGER PRIMARY KEY, name TEXT NOT NULL UNIQUE, is_default BOOLEAN NOT NULL DEFAULT 0);",
    "CREATE TABLE IF NOT EXISTS files (id INTEGER PRIMARY KEY, name TEXT NOT NULL, path TEXT NOT NULL, shelf_id INTEGER NOT NULL, FOREIGN KEY (shelf_id) REFERENCES shelves(id) ON DELETE CASCADE ON UPDATE CASCADE, UNIQUE (name, shelf_id));",
    "INSERT OR IGNORE INTO shelves (name, id, is_default) VALUES ('default', 0, 1);"
  ]

initDb :: SQL.Connection -> IO ()
initDb conn = mapM_ (\sql -> SQL.execute_ conn $ SQL.Query sql) dbTables

connect :: T.Shelf -> ConnectionString -> ConnectionType -> DBAction Context
connect shelf conn_str conn_type = (liftIO getPath) >>= \p -> createContext $ ContT (SQL.withConnection p) >>= \conn -> liftIO $ (initAndGetShelfId conn) >> return conn
  where
    getPath = case conn_str of
      Path p -> checkAndEnsureExists p >> return p
      InMemory -> return ":memory:"
      DefaultDB -> dbPath
    checkAndEnsureExists p = DIR.doesFileExist p >>= ensureExists p
    initAndGetShelfId = case conn_type of
      Normal -> \ctx -> initDb ctx
      DryRun -> \ctx -> return ()
    createContext conn = optimiseContext $ Context conn shelf isDryRun
    isDryRun = case conn_type of
      DryRun -> True
      _ -> False
    ensureExists p exists =
      if exists
        then return ()
        else DIR.createDirectoryIfMissing True p

getFiles :: Text -> Context -> DBAction [Entry]
getFiles name context = (\rs -> map handler rs) <$> (res =<< targetShelfId context)
  where
    res = \id -> query context "SELECT name, path FROM files WHERE name = ? AND shelf_id = ?" (name, id)
    handler = \(name, path) -> Entry name path (target_shelf context)

getAllFiles :: Context -> DBAction [Entry]
getAllFiles context = (\res -> map handler res) <$> (stmt =<< targetShelfId context)
  where
    stmt = \id -> query context "SELECT name, path, shelf_id FROM files WHERE shelf_id = ?" (Only id)
    handler = \(n, p, sid) -> Entry {name = n, path = p, shelf_id = ShelfID sid}

mapToShelves :: [Text] -> [Shelf]
mapToShelves = map (\name -> ShelfName name)

removeFile :: Text -> Context -> DBAction ()
removeFile name context = stmt =<< targetShelfId context
  where
    stmt = \id -> execute context "DELETE FROM files WHERE name = ? AND shelf_id = ?" (name, id)

getShelfName :: Integer -> Context -> DBAction Text
getShelfName sid context = (\ns -> (\ns2 -> ns2 !! 0) ns !! 0) <$> query context "SELECT name FROM shelves WHERE id = ?" (Only sid)

getShelfId :: Text -> Context -> DBAction Integer
getShelfId shelf_name context = nestedNth 0 <$> query context "SELECT id FROM shelves WHERE name = ?" (Only shelf_name)

nestedNth :: Int -> [[a]] -> a
nestedNth n as = (\as2 -> as2 !! n) as !! n

defaultShelfId :: Context -> DBAction Integer
defaultShelfId context = nestedNth 0 <$> (conn context >>= \c -> liftIO $ SQL.query_ c "SELECT id FROM shelves WHERE is_default = 1")

makeEntry :: Text -> Prelude.FilePath -> Context -> IO Entry
makeEntry name path context = ctor <$> getDir
  where
    getDir = (\p -> pack p) <$> (DIR.makeAbsolute path)
    ctor = \dir -> Entry name dir (target_shelf context)

defaultShelfName :: Text
defaultShelfName = "default"

getAllShelfNames :: Context -> DBAction [Text]
getAllShelfNames ctx = (\rs -> map (\rs2 -> rs2 !! 0) rs) <$> (conn ctx >>= \c -> liftIO $ SQL.query_ c "SELECT name FROM shelves")

removeShelf :: Text -> Context -> DBAction ()
removeShelf name ctx = execute ctx "DELETE FROM shelves WHERE name = ? AND is_default = 0" (Only name)

changeTargetShelf :: Shelf -> Context -> Context
changeTargetShelf shelf ctx = ctx {target_shelf = shelf}

copyEntryTo :: Shelf -> Text -> Context -> DBAction ()
copyEntryTo to_shelf name ctx = (\to_ctx -> getFiles name ctx >>= \files -> doInsert (files !! 0) to_ctx) $ changeTargetShelf to_shelf ctx
  where
    fixShelf = \f to_ctx -> f {shelf_id = target_shelf to_ctx}
    doInsert = \file to_ctx -> insert (fixShelf file to_ctx) to_ctx

copyEntry :: Shelf -> Shelf -> Text -> Context -> DBAction ()
copyEntry from to entry ctx = getEntries >>= \entries -> checkUnique entries to_ctx >> insertMany entries to_ctx
  where
    getEntries = retrieveAllLike entry from_ctx :: DBAction [T.Entry]
    from_ctx = changeTargetShelf from ctx
    to_ctx = changeTargetShelf to ctx
    checkUnique :: [T.Entry] -> Context -> DBAction ()
    checkUnique entries ctx =
      (checkAllUnique entries ctx)
        >>= \all_unique ->
          liftIO $
            if all_unique
              then return ()
              else throw $ EX.NamingConflict $ EX.Entry entry

wipeDb :: Context -> DBAction ()
wipeDb ctx =
  if (doing_dryrun ctx)
    then return ()
    else conn ctx >>= \c -> liftIO $ SQL.execute_ c "DROP TABLE shelves" >> SQL.execute_ c "DROP TABLE files" >> initDb c

targetShelfId :: Context -> DBAction Integer
targetShelfId ctx = dbId (target_shelf ctx) ctx

dummyShelf :: Shelf
dummyShelf = ShelfID 0

defaultShelf :: Shelf
defaultShelf = ShelfName "default"

setSecondD :: [[a]] -> [a]
setSecondD = map (\list -> list !! 0)

rsToFile :: (Text, Text, Integer) -> Entry
rsToFile (name, path, shelf) = Entry name path (ShelfID shelf)

formatLikeExpr :: Text -> Text
formatLikeExpr = T.map repl
  where
    repl '*' = '%'
    repl ch = ch

checkUnique entry ctx = not <$> DB.exists entry ctx

checkAllUnique :: (DBObject a) => [a] -> Context -> DBAction Bool
checkAllUnique entries ctx = mapM (\ent -> checkUnique ent ctx) entries >>= \all_unique -> return $ all (\b -> b) all_unique

optimiseContext :: Context -> DBAction Context
optimiseContext ctx = targetShelfId ctx >>= \id -> return ctx {target_shelf = ShelfID id}
