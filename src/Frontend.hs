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

import Control.Monad.Except (catchError, liftIO, runExceptT)
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

data ArgsResult
  = ArgsResult Command (Maybe Text) (Maybe Text)

data ShelfArgs
  = AddShelf Text
  | RemoveShelf Text Bool
  | ListShelves
  | RenameShelf Text Text
  | ExportShelf (Maybe FilePath)
  | ImportShelf (Maybe FilePath)

data Command
  = AddCmd Text Text
  | List Bool (Maybe Text)
  | Remove Text Bool
  | ShelfCmd ShelfArgs
  | CopyCmd Text Text Text
  | MoveCmd Text Text Text
  | RenameCmd Text Text
  | VersionCmd
  | ConfigCmd

add_opts :: Parser Command
add_opts =
  AddCmd
    <$> argument str (metavar "Path")
    <*> strOption
      ( long "name"
          <> short 'n'
          <> help "Set name for a new or changed target"
      )

generate_parse_info :: Parser Command
generate_parse_info = parse_details --"Shelve your files to refer to them quickly later"
  where
    parse_details =
      (subparser . foldMap cmd_info)
        [ ("add", "Add an entry to the shelf", add_opts),
          ("list", "List entries on the shelf", list_opts),
          ("remove", "Remove an entry from the shelf", remove_opts),
          ("shelves", "Operate on shelves", generateShelfOptions),
          ("move", "Move an entry to a different shelf", moveEntryOpts),
          ("copy", "Copy an entry to a different shelf", copyEntryOpts),
          ("rename", "Rename an entry", renameEntryOpts),
          ("fp", "Print entry with path, shorthand for: list --path --name <TARGET>", pathPrintOpts),
          ("version", "Print version information", pure VersionCmd)
        ]

    pathPrintOpts =
      (\arg -> List False (Just arg)) <$> strArgument (metavar "TARGET")

    renameEntryOpts =
      RenameCmd
        <$> strOption (long "from" <> short 'f' <> help "Source name")
        <*> strOption (long "to" <> short 't' <> help "Target name")

    list_opts =
      List
        <$> switch
          ( long "full"
              <> help "Show full information about each entry (name, shelf, etc)"
          )
        <*> ( optional $
                strOption
                  ( long "name"
                      <> short 'n'
                      <> help "Show entries with matching name(s)"
                  )
            )
    remove_opts =
      Remove
        <$> argument str (metavar "NAME") <*> noConfirmSwitch

    moveCopyOpts = strOption (long "from" <> short 'f' <> help "Source shelf")
    moveCopyOpts2 = strOption (long "to" <> short 't' <> help "Destination shelf")
    moveCopyOpts3 = strArgument (metavar "TARGET")
    moveEntryOpts = MoveCmd <$> moveCopyOpts <*> moveCopyOpts2 <*> moveCopyOpts3
    copyEntryOpts = CopyCmd <$> moveCopyOpts <*> moveCopyOpts2 <*> moveCopyOpts3
    generateShelfOptions =
      ShelfCmd
        <$> (subparser . foldMap cmd_info)
          [ ("add", "Add an new shelf", shelf_add_opts),
            ("remove", "Remove a shelf", shelf_remove_opts),
            ("list", "List all shelves", pure ListShelves),
            ("rename", "Rename a shelf", renameShelfOpts),
            ("import", "Import a shelf from a file", ImportShelf <$> optionalFilePathArg),
            ("export", "export a shelf to a file", ExportShelf <$> optionalFilePathArg)
          ]

    filePathArg = strArgument (metavar "FILE")
    optionalFilePathArg = optional filePathArg

    noConfirmSwitch = switch (long "no-confirm" <> short 'y' <> help "Auto accept all confirmation dialogs")
    shelf_add_opts =
      AddShelf <$> strArgument (metavar "NAME")
    shelf_remove_opts =
      RemoveShelf <$> strArgument (metavar "NAME") <*> noConfirmSwitch
    renameShelfOpts =
      RenameShelf <$> strOption (long "from" <> short 'f' <> help "Source name")
        <*> strOption (long "to" <> short 't' <> help "Target name")

    info' p desc = info (helper <*> p) (fullDesc <> progDesc desc)
    cmd_info (cmd_name, desc, parser) = command cmd_name (info' parser desc)

generateArgsInfo :: ParserInfo ArgsResult
generateArgsInfo = makeArgsInfo generate_parse_info

makeArgsInfo :: Parser Command -> ParserInfo ArgsResult
makeArgsInfo cmd = info (args <**> helper) (fullDesc <> progDesc "")
  where
    args = cmdArgs

    cmdArgs =
      ArgsResult
        <$> cmd
        <*> ( optional $
                strOption
                  ( long "shelf"
                      <> help "The shelf to use"
                  )
            )
        <*> ( optional $
                strOption
                  ( long "database-path"
                      <> help "Path to custom database"
                  )
            )

parseOptions :: ArgsResult -> EX.Exception IO ()
parseOptions (ArgsResult cmd shelf_name db_path) = FSM.connectToDb (Just shelf) db_path >>= parseCommand cmd
  where
    shelf = case shelf_name of
      Just name -> T.ShelfName name
      Nothing -> DB.defaultShelf

parseCommand :: Command -> DB.Context -> EX.Exception IO ()
parseCommand (List path_only match) ctx = liftIO $ FSM.getAllEntries ctx match >>= pathsPrinter path_only
parseCommand (AddCmd target name) ctx = (liftIO $ DB.makeEntry name target ctx) >>= \entry -> FSM.addEntry entry ctx
parseCommand (Remove name no_confirm) ctx = liftIO $ getConfirm >>= removeDecider
  where
    getConfirm =
      if no_confirm
        then return True
        else getConfirmationYesNo "This will permanently delete this entry "
    removeDecider remove =
      if remove
        then DB.removeFile name ctx
        else return ()
parseCommand (ShelfCmd cmd) ctx = parseShelvesCmd cmd ctx
parseCommand (MoveCmd from to name) ctx = FSM.moveEntryByName from to name ctx
parseCommand (CopyCmd from to name) ctx = FSM.copyEntry (T.ShelfName from) (T.ShelfName to) name ctx
parseCommand (RenameCmd from to) ctx = FSM.renameEntryByName from to ctx
parseCommand (VersionCmd) _ = liftIO Pretty.printVersionInfo

parseShelvesCmd :: ShelfArgs -> DB.Context -> EX.Exception IO ()
parseShelvesCmd (AddShelf name) ctx = FSM.addShelf (T.ShelfName name) ctx
parseShelvesCmd (RemoveShelf name no_confirm) ctx = liftIO $ getConfirm >>= removeDecider
  where
    getConfirm =
      if no_confirm
        then return True
        else getConfirmationYesNo "This will permanently delete this shelf and all its entries"
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
getConfirmationYesNo prompt = printf "%s are you sure? [y/n]: " prompt >> hFlush stdout >> getLine >>= checkLine
  where
    checkLine "yes" = return True
    checkLine "y" = return True
    checkLine "no" = return False
    checkLine "n" = return False
    checkLine _ = putStrLn "Please enter y or n" >> getConfirmationYesNo prompt

setupFile :: Text -> DB.Context -> T.Entry
setupFile name ctx = T.Entry name "" (DB.target_shelf ctx)
