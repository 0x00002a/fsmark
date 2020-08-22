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
    DBObject (insert, remove, retrieveAll, exists, dbId, rename, retrieveAllLike),
    File (path, name, File),
    Shelf (ShelfName),
    Context (target_shelf),
    makeFile,
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
  )
where

import Control.Monad (forM)
import Data.Text (Text, append, pack, unpack)
import qualified Data.Text as T
import Database.SQLite.Simple
import qualified Pretty
import qualified System.Directory as DIR
import System.FilePath ((</>))
import Text.Printf (printf)

class DBObject a where
  exists :: a -> Context -> IO Bool
  insert :: a -> Context -> IO ()
  dbId :: a -> Context -> IO Integer
  remove :: a -> Context -> IO ()
  retrieveAll :: Context -> IO [a]
  rename :: a -> Text -> Context -> IO ()
  getName :: a -> Context -> IO Text
  retrieveAllLike :: Text -> Context -> IO [a]

data Context = Context
  { conn :: Connection,
    target_shelf :: Shelf
  }

data File = File
  { name :: Text,
    path :: Text,
    shelf_id :: Shelf
  }

data Shelf
  = ShelfID Integer
  | ShelfName Text
  deriving (Show, Eq)

instance DBObject Shelf where
  insert (ShelfName name) ctx = execute (conn ctx) "INSERT INTO shelves (name) VALUES (?)" (Only name)
  dbId (ShelfID id) _ = return id
  dbId (ShelfName name) ctx = getShelfId name (conn ctx)
  remove (ShelfID id) ctx = getShelfName id ctx >>= \name -> removeShelf name ctx
  remove (ShelfName name) ctx = removeShelf name ctx
  retrieveAll ctx = (\names -> map (\name -> ShelfName name) names) <$> getAllShelfNames ctx
  exists (ShelfName name) ctx = nestedNth 0 <$> query (conn ctx) "SELECT EXISTS (SELECT 1 FROM shelves WHERE name = ?)" (Only name)
  exists (ShelfID id) ctx = nestedNth 0 <$> query (conn ctx) "SELECT EXISTS (SELECT 1 FROM shelves WHERE id = ?)" (Only id)
  rename (ShelfName name) to ctx = execute (conn ctx) "UPDATE shelves SET name = ? WHERE name = ?" (to, name)
  rename (ShelfID id) to ctx = execute (conn ctx) "UPDATE shelves SET name = ? WHERE id = ?" (to, id)
  getName (ShelfName name) _ = return name
  getName (ShelfID id) ctx = getShelfName id ctx
  retrieveAllLike name ctx = (\names -> map (\name -> ShelfName name) (setSecondD names)) <$> query (conn ctx) "SELECT name FROM shelves WHERE name LIKE ?" (Only (formatLikeExpr name))

instance DBObject File where
  insert file ctx = execQuery =<< targetShelfId ctx
    where
      execQuery = \id -> execute (conn ctx) "INSERT INTO files (name, path, shelf_id) VALUES (?, ?, ?)" $ args id
      args = \id -> ((name file), (path file), id)

  dbId file ctx = nestedNth 0 <$> (execQuery =<< targetShelfId ctx)
    where
      execQuery = \id -> query (conn ctx) "SELECT id FROM files WHERE name = ? AND shelf_id = ?" ((name file), id)
  remove file ctx = removeFile (name file) ctx
  retrieveAll ctx = getAllFiles ctx
  exists file ctx = nestedNth 0 <$> ((\id -> query (conn ctx) "SELECT EXISTS (SELECT 1 FROM files WHERE name = ? AND shelf_id = ?)" ((name file), id)) =<< targetShelfId ctx)
  rename file to ctx = (\id -> execute (conn ctx) "UPDATE files SET name = ? WHERE name = ? AND shelf_id = ?" (to, (name file), id)) =<< dbId (shelf_id file) ctx
  getName file _ = return $ name file
  retrieveAllLike name ctx = (\res -> map rsToFile res) <$> execQuery
    where
      execQuery = targetShelfId ctx >>= \id -> query (conn ctx) "SELECT name, path, shelf_id FROM files WHERE name LIKE ? AND shelf_id = ?" (formatLikeExpr name, id)

instance Pretty.PrettyPrintable File where
  display f = printf "######\nName: %s\nPath: %s\n" (name f) (path f)

instance Show File where
  show f = "Name: " ++ unpack (name f) ++ "\nPath: " ++ unpack (path f)

dbPath :: IO FilePath
dbPath = (\p -> p </> "data.db") <$> dir
  where
    dir = DIR.getXdgDirectory DIR.XdgData "fsm" >>= (\p -> (\() -> p) <$> DIR.createDirectoryIfMissing True p)

dbTables :: [Text]
dbTables =
  [ "CREATE TABLE IF NOT EXISTS shelves (id INTEGER PRIMARY KEY, name TEXT NOT NULL UNIQUE, is_default BOOLEAN NOT NULL DEFAULT 0);",
    "CREATE TABLE IF NOT EXISTS files (id INTEGER PRIMARY KEY, name TEXT NOT NULL, path TEXT NOT NULL, shelf_id INTEGER NOT NULL, FOREIGN KEY (shelf_id) REFERENCES shelves(id) ON DELETE CASCADE ON UPDATE CASCADE, UNIQUE (name, shelf_id));",
    "INSERT INTO shelves (name, id, is_default) VALUES ('default', 0, 1);"
  ]

initDb :: Connection -> Bool -> IO ()
initDb conn False = mapM_ (\sql -> execute_ conn $ Query sql) dbTables
initDb _ _ = return ()

connect :: Text -> Maybe Text -> IO Context
connect shelf_name connection_path =
  getPath >>= \p ->
    DIR.doesFileExist p
      >>= ( \exists ->
              open p
                >>= (\conn -> createContext conn <$> initAndGetShelfId exists conn)
          )
  where
    initAndGetShelfId = \exists ctx -> initDb ctx exists >> (\id -> ShelfID id) <$> getShelfId shelf_name ctx
    createContext = Context
    getPath = case connection_path of
      Just p -> return $ unpack p
      Nothing -> dbPath

getFiles :: Text -> Context -> IO [File]
getFiles name context = (\rs -> map handler rs) <$> (res =<< targetShelfId context)
  where
    res = \id -> query (conn context) "SELECT name, path FROM files WHERE name = ? AND shelf_id = ?" (name, id)
    handler = \(name, path) -> File name path (target_shelf context)

getAllFiles :: Context -> IO [File]
getAllFiles context = (\res -> map handler res) <$> (stmt =<< targetShelfId context)
  where
    stmt = \id -> query (conn context) "SELECT name, path, shelf_id FROM files WHERE shelf_id = ?" (Only id)
    handler = \(n, p, sid) -> File {name = n, path = p, shelf_id = ShelfID sid}

mapToShelves :: [Text] -> [Shelf]
mapToShelves = map (\name -> ShelfName name)

removeFile :: Text -> Context -> IO ()
removeFile name context = stmt =<< targetShelfId context
  where
    stmt = \id -> execute (conn context) "DELETE FROM files WHERE name = ? AND shelf_id = ?" (name, id)

getShelfName :: Integer -> Context -> IO Text
getShelfName sid context = (\ns -> (\ns2 -> ns2 !! 0) ns !! 0) <$> query (conn context) "SELECT name FROM shelves WHERE id = ?" (Only sid)

getShelfId :: Text -> Connection -> IO Integer
getShelfId shelf_name context = nestedNth 0 <$> query context "SELECT id FROM shelves WHERE name = ?" (Only shelf_name)

nestedNth :: Int -> [[a]] -> a
nestedNth n as = (\as2 -> as2 !! n) as !! n

defaultShelfId :: Context -> IO Integer
defaultShelfId context = nestedNth 0 <$> query_ (conn context) "SELECT id FROM shelves WHERE is_default = 1"

makeFile :: Text -> Text -> Context -> IO File
makeFile name path context = ctor <$> getDir
  where
    getDir = (\p -> pack p) <$> (DIR.makeAbsolute $ unpack path)
    ctor = \dir -> DB.File name dir (target_shelf context)

defaultShelfName :: Text
defaultShelfName = "default"

getAllShelfNames :: Context -> IO [Text]
getAllShelfNames ctx = (\rs -> map (\rs2 -> rs2 !! 0) rs) <$> query_ (conn ctx) "SELECT name FROM shelves"

removeShelf :: Text -> Context -> IO ()
removeShelf name ctx = execute (conn ctx) "DELETE FROM shelves WHERE name = ? AND is_default = 0" (Only name)

changeTargetShelf :: Shelf -> Context -> Context
changeTargetShelf shelf ctx = ctx {target_shelf = shelf}

copyEntryTo :: Shelf -> Text -> Context -> IO ()
copyEntryTo to_shelf name ctx = (\to_ctx -> DB.getFiles name ctx >>= \files -> doInsert (files !! 0) to_ctx) $ changeTargetShelf to_shelf ctx
  where
    fixShelf = \f to_ctx -> f {shelf_id = target_shelf to_ctx}
    doInsert = \file to_ctx -> insert (fixShelf file to_ctx) to_ctx

wipeDb :: Context -> IO ()
wipeDb ctx = execute_ (conn ctx) "DROP TABLE shelves" >> execute_ (conn ctx) "DROP TABLE files" >> initDb (conn ctx) False

targetShelfId :: Context -> IO Integer
targetShelfId ctx = dbId (target_shelf ctx) ctx

dummyShelf :: Shelf
dummyShelf = ShelfID 0

defaultShelf :: Shelf
defaultShelf = ShelfName "default"

setSecondD :: [[a]] -> [a]
setSecondD = map (\list -> list !! 0)

rsToFile :: (Text, Text, Integer) -> File
rsToFile (name, path, shelf) = File name path (ShelfID shelf)

formatLikeExpr :: Text -> Text
formatLikeExpr = T.map repl
  where
    repl '*' = '%'
    repl ch = ch
