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

data ArgsResult
  = ArgsResult Command (Maybe TargetShelfArg) (Maybe Text)

data TargetShelfArg
  = StoredShelf Text
  | HotLoadedShelf FilePath

data ShelfArgs
  = AddShelf Text
  | RemoveShelf Text Bool
  | ListShelves
  | RenameShelf Text Text
  | ExportShelf (Maybe FilePath)
  | ImportShelf (Maybe FilePath)
  | ShelvesHelp Command

data Command
  = AddCmd FilePath (Maybe Text) Bool
  | List Bool (Maybe Text)
  | Remove Text Bool
  | ShelfCmd ShelfArgs
  | CopyCmd Text Text Text
  | MoveCmd Text Text Text
  | RenameCmd Text Text
  | VersionCmd
  | ConfigCmd
  | HelpCmd
  | ViewLicenseCmd

generate_parse_info :: Parser Command
generate_parse_info = parse_details --"Shelve your files to refer to them quickly later"
  where
    parse_details =
      (subparser . foldMap cmd_info)
        [ ("add", "Add an entry to the current shelf", add_opts),
          ("list", "List entries on the current shelf", list_opts),
          ("remove", "Remove an entry from the current shelf", remove_opts),
          ("shelves", "Operate on shelves", generateShelfOptions),
          ("move", "Move an entry to a different shelf", moveEntryOpts),
          ("copy", "Copy an entry to a different shelf", copyEntryOpts),
          ("rename", "Rename an entry", renameEntryOpts),
          ("fp", "Print the full path for an entry", pathPrintOpts),
          ("version", "Print version information", pure VersionCmd),
          ("license", "Print license information", pure ViewLicenseCmd)
        ]

    pathPrintOpts =
      (\arg -> List False (Just arg)) <$> strArgument (metavar "TARGET")

    renameEntryOpts = fromToOpts RenameCmd

    fromToOpts target = target <$> fromArg <*> toArg
      where
        fromArg = strOption (long "from" <> short 'f' <> help "Source name" <> metavar "SOURCE")
        toArg = strOption (long "to" <> short 't' <> help "Target name" <> metavar "TARGET")

    list_opts =
      List
        <$> switch
          ( long "full"
              <> short 'f'
              <> help "Show full information about each entry (name, path, etc)"
          )
        <*> ( optional $
                strOption
                  ( long "search"
                      <> short 's'
                      <> help "Filter results by a search term. Understands [%|*] as wildcard, i.e name* or name% will match anything starting with name, %name% or *name* will matching anything containing name"
                  )
            )
    remove_opts =
      Remove
        <$> argument str (metavar "NAME") <*> noConfirmSwitch

    add_opts =
      AddCmd
        <$> argument str (metavar "PATH")
        <*> ( optional $
                strOption
                  ( long "name"
                      <> short 'n'
                      <> help "Set a name for the target (removes name confirmation dialog)"
                  )
            )
        <*> noConfirmSwitch

    targetArg = strArgument (metavar "ENTRY NAME")
    moveEntryOpts = fromToOpts MoveCmd <*> targetArg
    copyEntryOpts = fromToOpts CopyCmd <*> targetArg
    generateShelfOptions :: Parser Command
    generateShelfOptions =
      ShelfCmd
        <$> (subparser . foldMap cmd_info)
          [ ("add", "Add an new shelf", shelf_add_opts),
            ("remove", "Remove a shelf", shelf_remove_opts),
            ("list", "List all shelves", pure ListShelves),
            ("rename", "Rename a shelf", renameShelfOpts),
            ("import", "Import a shelf from a file or stdin", ImportShelf <$> optionalFilePathArg),
            ("export", "Export a shelf to a file or stdout", ExportShelf <$> optionalFilePathArg)
          ]

    filePathArg = strArgument (metavar "FILE")
    optionalFilePathArg = optional filePathArg

    noConfirmSwitch = switch (long "no-confirm" <> short 'y' <> help "Auto accept all confirmation dialogs")
    shelf_add_opts =
      AddShelf <$> strArgument (metavar "NAME")
    shelf_remove_opts =
      RemoveShelf <$> strArgument (metavar "NAME") <*> noConfirmSwitch
    renameShelfOpts = fromToOpts RenameShelf

    info' p desc = info (helper <*> p) (fullDesc <> progDesc desc)
    cmd_info (cmd_name, desc, parser) = command cmd_name (info' parser desc)

generateArgsInfo :: ParserInfo ArgsResult
generateArgsInfo = makeArgsInfo generate_parse_info

fsmDesc =
  progDesc "A bookmarking system for your filesystem"
    <> footer "'fsm <command> --help' can be used to view more specific help for each command"

makeArgsInfo :: Parser Command -> ParserInfo ArgsResult
makeArgsInfo cmd = info (args <**> helper) (fullDesc <> fsmDesc)
  where
    args = cmdArgs

    cmdArgs =
      ArgsResult
        <$> cmd
        <*> (optional $ internalShelfArg)
        <*> ( optional $
                strOption
                  ( long "database"
                      <> help "Path to custom database (will be created if it does not exist)"
                  )
            )

    internalShelfArg =
      StoredShelf
        <$> strOption
          ( long "shelf"
              <> short 's'
              <> help "The shelf to use"
          )
    hotLoadedShelfArg =
      HotLoadedShelf
        <$> strOption
          ( long "shelf-from"
              <> help "Load a shelf temporarily from exported JSON"
          )

parseOptions :: ArgsResult -> EX.Exception IO ()
parseOptions (ArgsResult cmd tshelf db_path) = parseTargetShelf db_path tshelf >>= parseCommand cmd

parseTargetShelf :: Maybe Text -> Maybe TargetShelfArg -> EX.Exception IO DB.Context
parseTargetShelf db_path (Just (StoredShelf name)) = FSM.connectToDb (Just (T.ShelfName name)) db_path
parseTargetShelf _ (Just (HotLoadedShelf path)) = FSM.connectToDbWithTmpShelf (Just path)
parseTargetShelf db_path Nothing = FSM.connectToDb Nothing db_path

parseCommand :: Command -> DB.Context -> EX.Exception IO ()
parseCommand (List path_only match) ctx = liftIO $ FSM.getAllEntries ctx match >>= pathsPrinter path_only
parseCommand (AddCmd path chosen_name no_confirm) ctx =
  liftIO getName
    >>= (\name -> liftIO $ DB.makeEntry name (pack path) ctx)
    >>= \entry -> FSM.addEntry entry ctx
  where
    getName = case chosen_name of
      Just n -> return n
      Nothing ->
        FSM.entryNameFromPathIfUnique path ctx
          >>= \name -> namePrompt name
    namePrompt name =
      (promptInput $ "Enter a name for the new entry" `append` (nameAutoComplete name))
        >>= \input_name -> decodeName input_name name
    nameAutoComplete (Just name) = " [" `append` name `append` "]: "
    nameAutoComplete Nothing = ": "
    decodeName ("") (Just name) = return name
    decodeName ("") Nothing = putStrLn "Please enter a valid name" >> namePrompt Nothing
    decodeName name _ = return name
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
parseShelvesCmd (ListShelves) ctx = liftIO $ DB.getAllShelfNames ctx >>= Pretty.printList
parseShelvesCmd (RenameShelf from to) ctx = FSM.renameShelfByName from to ctx
parseShelvesCmd (ImportShelf path) ctx = FSM.importShelf path ctx
parseShelvesCmd (ExportShelf path) ctx = FSM.exportShelf (DB.target_shelf ctx) ctx path

extractPaths :: [T.Entry] -> [Text]
extractPaths files = map (\f -> DB.path f) files

pathsPrinter :: Bool -> [T.Entry] -> IO ()
pathsPrinter False files = Pretty.printList $ extractPaths files
pathsPrinter True files = Pretty.printList files

getConfirmationYesNo :: Text -> IO Bool
getConfirmationYesNo prompt = printf "%s [y/n]: " prompt >> hFlush stdout >> getLine >>= checkLine
  where
    checkLine "yes" = return True
    checkLine "y" = return True
    checkLine "no" = return False
    checkLine "n" = return False
    checkLine _ = putStrLn "Please enter y or n" >> getConfirmationYesNo prompt

setupFile :: Text -> DB.Context -> T.Entry
setupFile name ctx = T.Entry name "" (DB.target_shelf ctx)

promptInput :: Text -> IO Text
promptInput prompt = putStr (unpack prompt) >> hFlush stdout >> getLine >>= \line -> return $ pack line