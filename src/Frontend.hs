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

import ArgsSetup
import Control.Monad.Except (catchError, liftIO, runExceptT, throwError)
import qualified DB
import Data.Text (Text, append, pack, unpack)
import qualified Exceptions as EX
import qualified FSM
import Options.Applicative
import qualified Pretty
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import qualified Types as T

runFSM :: IO ()
runFSM = customExecParser p generateArgsInfo >>= \options -> (runExceptT $ parseOptions options) >>= handleErrors
  where
    p = prefs (showHelpOnEmpty <> disambiguate)

handleErrors :: Either EX.Error a -> IO ()
handleErrors err = case err of
  Left err -> EX.printError err
  Right _ -> return ()

handleErrorsT :: EX.Exception IO a -> IO ()
handleErrorsT err = runExceptT err >>= handleErrors

parseOptions :: ArgsResult -> EX.Exception IO ()
parseOptions (ArgsResult cmd tshelf db_path dry_run) = parseTargetShelf db_path tshelf dry_run >>= parseCommand cmd
  where
    parseTargetShelf db_path (Just (StoredShelf name)) = FSM.connectToDb (Just (T.ShelfName name)) db_path
    parseTargetShelf db_path Nothing = FSM.connectToDb Nothing db_path

parseCommand :: Command -> DB.Context -> EX.Exception IO ()
parseCommand (List path_only match) ctx = liftIO $ FSM.getAll ctx match >>= pathsPrinter path_only
parseCommand (AddCmd path chosen_name no_confirm) ctx = createEntryFromInput chosen_name path ctx no_confirm >>= \entry -> FSM.addEntry entry ctx
parseCommand (Remove name no_confirm) ctx = liftIO $ getConfirm >>= removeDecider
  where
    getConfirm =
      if no_confirm
        then return True
        else getConfirmationYesNo "This will permanently remove this entry, are you sure?"
    removeDecider remove =
      if remove
        then DB.removeFile name ctx
        else return ()
parseCommand (ShelfCmd cmd) ctx = parseShelvesCmd cmd ctx
parseCommand (MoveCmd from to name) ctx = FSM.moveEntryByName from to name ctx
parseCommand (CopyCmd from to name) ctx = FSM.copyEntry (T.ShelfName from) (T.ShelfName to) name ctx
parseCommand (RenameCmd from to) ctx = FSM.renameEntryByName from to ctx
parseCommand (VersionCmd) _ = liftIO Pretty.printVersionInfo
parseCommand (ViewLicenseCmd) _ = liftIO Pretty.printLicense

parseShelvesCmd :: ShelfArgs -> DB.Context -> EX.Exception IO ()
parseShelvesCmd (AddShelf name) ctx = FSM.addShelf (T.ShelfName name) ctx
parseShelvesCmd (RemoveShelf name no_confirm) ctx = liftIO $ getConfirm >>= removeDecider
  where
    getConfirm =
      if no_confirm
        then return True
        else getConfirmationYesNo "This will permanently remove this shelf and all of it's entries, are you sure?"
    removeDecider remove =
      if remove
        then DB.removeShelf name ctx
        else return ()
parseShelvesCmd (ListShelves) ctx = liftIO $ (FSM.getAll ctx Nothing :: IO [T.Shelf]) >>= Pretty.printList
parseShelvesCmd (RenameShelf from to) ctx = FSM.renameShelfByName from to ctx
parseShelvesCmd (ImportShelf path) ctx = FSM.importShelf path ctx
parseShelvesCmd (ExportShelf name path) ctx = FSM.exportShelf (T.ShelfName name) ctx path

extractPaths :: [T.Entry] -> [Text]
extractPaths files = map (\f -> DB.path f) files

pathsPrinter :: Bool -> [T.Entry] -> IO ()
pathsPrinter False files = Pretty.printList $ extractPaths files
pathsPrinter True files = Pretty.printList files

getConfirmationYesNo :: Text -> IO Bool
getConfirmationYesNo prompt = promptInput (prompt `append` "[y/n]: ") >>= checkLine
  where
    checkLine "yes" = return True
    checkLine "y" = return True
    checkLine "no" = return False
    checkLine "n" = return False
    checkLine _ = putStrLn "Please enter y or n" >> getConfirmationYesNo prompt

promptInput :: Text -> IO Text
promptInput prompt = putStr (unpack prompt) >> hFlush stdout >> getLine >>= \line -> return $ pack line

createEntryFromInput name path db_ctx confirm =
  liftIO getName
    >>= (\name -> liftIO $ DB.makeEntry name path db_ctx)
  where
    getName = case name of
      Just n -> return n
      Nothing ->
        FSM.entryNameFromPathIfUnique path db_ctx
          >>= \name ->
            if confirm
              then namePrompt name
              else case name of
                Just nm -> return nm
                Nothing -> namePrompt name

    namePrompt name =
      (promptInput $ "Enter a name for the new entry" `append` (nameAutoComplete name))
        >>= \input_name -> decodeName input_name name
    nameAutoComplete (Just name) = " [" `append` name `append` "]: "
    nameAutoComplete Nothing = ": "
    decodeName ("") (Just name) = return name
    decodeName ("") Nothing = putStrLn "Please enter a valid name" >> namePrompt Nothing
    decodeName name _ = return name
