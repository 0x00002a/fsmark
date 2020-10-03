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

module Frontend where

import           ArgsSetup
import           Control.Exception              ( throw
                                                , try
                                                )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import qualified DB
import           Data.Maybe                     ( catMaybes
                                                , mapMaybe
                                                )
import           Data.Text                      ( Text
                                                , append
                                                , pack
                                                , unpack
                                                )
import qualified Exceptions                    as EX
import qualified FSM
import           Options.Applicative
import qualified Pretty
import           System.IO                      ( IOMode(..)
                                                , hFlush
                                                , hPutStr
                                                , stdout
                                                )
import           Text.Printf                    ( printf )
import qualified Types                         as T
import           Control.Monad                  ( unless
                                                , when
                                                )
import qualified Sys

runFSM :: IO ()
runFSM = customExecParser p generateArgsInfo
    >>= \options -> try (parseOptions options) >>= handleExceptions
  where
    handleExceptions :: Either EX.Error () -> IO ()
    handleExceptions (Left  exception) = print exception
    handleExceptions (Right _        ) = return ()
    p = prefs (showHelpOnEmpty <> disambiguate)

handleErrors :: Either EX.Error a -> IO ()
handleErrors err = case err of
    Left  err -> EX.printError err
    Right _   -> return ()

parseOptions :: ArgsResult -> DB.DBAction ()
parseOptions (ArgsResult cmd tshelf db_path dry_run) =
    parseTargetShelf db_path tshelf dry_run >>= parseCommand cmd
  where
    parseTargetShelf db_path (Just name) =
        FSM.connectToDb (Just (T.ShelfName name)) db_path
    parseTargetShelf db_path Nothing = FSM.connectToDb Nothing db_path

parseCommand :: Command -> DB.Context -> DB.DBAction ()
parseCommand (List path_only match) ctx = FSM.getAll ctx match >>= printItems
  where
    printItems [] = case match of
        Just m ->
            liftIO
                $        throw
                $        EX.TextError
                $        "No items matching '"
                `append` m
                `append` "'"
        Nothing -> return ()
    printItems items = liftIO $ pathsPrinter path_only items
parseCommand (AddCmd [path] chosen_name no_confirm False _) ctx = getEntry
    >>= \ent -> FSM.addEntry ent ctx
  where
    getEntry :: IO T.Entry
    getEntry = case chosen_name of
        Just name -> FSM.makeEntry name path
        Nothing   -> createEntryFromInput path ctx no_confirm

parseCommand (AddCmd paths _ no_confirm False _) ctx =
    createEntries >>= (`FSM.addMany` ctx)
  where
    createEntries :: IO [T.Entry]
    createEntries =
        mapM (\path -> createEntryFromInput path ctx no_confirm) paths
parseCommand (AddCmd paths chosen_name no_confirm True depth) ctx =
    createEntries >>= \entries -> FSM.addMany entries ctx
  where
    findEntries :: DB.DBAction [T.Entry]
    findEntries = concat <$> mapM (`FSM.createEntriesRecursive` depth) paths
    createEntries :: DB.DBAction [T.Entry]
    createEntries = findEntries >>= filterEntries
    filterEntries :: [T.Entry] -> DB.DBAction [T.Entry]
    filterEntries entries = if no_confirm
        then FSM.generateMapExists ctx entries >>= \mapped ->
            printFailedEntries mapped >> return
                (fst $ FSM.extractDuplicates $ FSM.notExistsOutOf mapped)
        else confirmNames entries
      where
        printFailedEntries :: [(T.Entry, Bool)] -> DB.DBAction ()
        printFailedEntries entries =
            mapM_
                    ( printf
                            "Cannot add '%s' because an entry with that name already exists\n"
                    . T.name
                    )
                $ nonUniqueEntries entries
        confirmNames = mapM promptEntry
        dupes        = FSM.extractDuplicates entries
        nonUniqueEntries entries = snd dupes ++ FSM.existsOutOf entries
    promptEntry :: T.Entry -> IO T.Entry
    promptEntry ent =
        (\n -> ent { T.name = n }) <$> namePrompt (Just $ T.name ent)
    setNames entries names =
        zipWith (\name entry -> entry { T.name = name }) names entries
parseCommand (Remove name no_confirm) ctx = liftIO getConfirm >>= removeDecider
  where
    getConfirm = if no_confirm
        then return True
        else getConfirmationYesNo
            "This will permanently remove this entry, are you sure?"
    removeDecider remove = when remove $ DB.removeFile name ctx
parseCommand (ShelfCmd cmd        ) ctx = parseShelvesCmd cmd ctx
parseCommand (MoveCmd from to name) ctx = FSM.moveEntryByName from to name ctx
parseCommand (CopyCmd from to name) ctx =
    FSM.copyEntryByName (T.ShelfName from) (T.ShelfName to) name ctx
parseCommand (RenameCmd from to) ctx = FSM.renameEntryByName from to ctx
parseCommand VersionCmd          _   = liftIO Pretty.printVersionInfo
parseCommand ViewLicenseCmd      _   = liftIO Pretty.printLicense

parseShelvesCmd :: ShelfArgs -> DB.Context -> DB.DBAction ()
parseShelvesCmd (AddShelf name) ctx = FSM.addShelf (T.ShelfName name) ctx
parseShelvesCmd (RemoveShelf name no_confirm) ctx =
    liftIO getConfirm >>= removeDecider
  where
    getConfirm = if no_confirm
        then return True
        else
            getConfirmationYesNo
                "This will permanently remove this shelf and all of it's entries, are you sure?"
    removeDecider remove = when remove $ DB.removeShelf name ctx
parseShelvesCmd ListShelves ctx =
    (FSM.getAll ctx Nothing :: DB.DBAction [T.Shelf])
        >>= \list -> liftIO $ Pretty.printList list
parseShelvesCmd (RenameShelf from to) ctx = FSM.renameShelfByName from to ctx
parseShelvesCmd (ImportShelf path   ) ctx = FSM.importShelf path ctx
parseShelvesCmd (ExportShelf name path) ctx =
    FSM.exportShelf (T.ShelfName name) ctx path

extractPaths :: [T.Entry] -> [Text]
extractPaths = map DB.path

pathsPrinter :: Bool -> [T.Entry] -> IO ()
pathsPrinter False files = Pretty.printList $ extractPaths files
pathsPrinter True  files = Pretty.printList files

getConfirmationYesNo :: Text -> IO Bool
getConfirmationYesNo prompt =
    promptInput (prompt `append` "[y/n]: ") >>= checkLine
  where
    checkLine "yes" = return True
    checkLine "y"   = return True
    checkLine "no"  = return False
    checkLine "n"   = return False
    checkLine _ = putStrLn "Please enter y or n" >> getConfirmationYesNo prompt

promptInput :: Text -> IO Text
promptInput prompt =
    putStr (unpack prompt) >> hFlush stdout >> getLine >>= \line ->
        return $ pack line

namePrompt name =
    promptInput
            ("Enter a name for the new entry" `append` nameAutoComplete name)
        >>= \input_name -> decodeName input_name name
  where
    nameAutoComplete (Just name) = " [" `append` name `append` "]: "
    nameAutoComplete Nothing     = ": "
    decodeName "" (Just name) = return name
    decodeName "" Nothing =
        putStrLn "Please enter a valid name" >> namePrompt Nothing
    decodeName name _ = return name

createEntryFromInput :: FilePath -> DB.Context -> Bool -> IO T.Entry
createEntryFromInput path db_ctx no_confirm =
    getName >>= (`FSM.makeEntry` path)
  where
    getName = if no_confirm
        then liftIO $ FSM.nameForPath path
        else doPrompt >>= \name -> liftIO $ namePrompt name

    doPrompt = FSM.entryNameFromPathIfUnique path db_ctx
