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

import qualified DB
import Data.Semigroup ((<>))
import Data.Text (Text, pack, unpack)
import Options.Applicative
import qualified Options.Applicative.Help as OH
import qualified Pretty
import qualified System.Directory as DIR
import System.Exit
import System.IO
import Text.Printf (printf)

runFSM :: IO ()
runFSM = customExecParser p generateArgsInfo >>= parseOptions
  where
    p = prefs (showHelpOnEmpty <> disambiguate)

data ArgsResult
  = ArgsResult Command (Maybe Text) (Maybe Text)

data ShelfArgs
  = AddShelf Text
  | RemoveShelf Text Bool
  | ListShelves
  | RenameShelf Text Text

data Command
  = AddCmd Text Text
  | List Bool (Maybe Text)
  | Remove Text Bool
  | ShelfCmd ShelfArgs
  | CopyCmd Text Text Text
  | MoveCmd Text Text Text
  | RenameCmd Text Text
  | VersionCmd

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
            ("rename", "Rename a shelf", renameShelfOpts)
          ]

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

parseOptions :: ArgsResult -> IO ()
parseOptions (ArgsResult cmd shelf_name db_path) = case shelf_name of
  Just shelf -> DB.connect shelf db_path >>= \db -> checkShelfExists shelf db >> parseCommand cmd db
  Nothing -> DB.connect DB.defaultShelfName db_path >>= parseCommand cmd

parseCommand :: Command -> DB.Context -> IO ()
parseCommand (List path_only (Just name)) ctx = DB.retrieveAllLike name ctx >>= \files -> pathsPrinter path_only files
parseCommand (List path_only Nothing) ctx = DB.retrieveAll ctx >>= \files -> pathsPrinter path_only files
parseCommand (AddCmd target name) ctx =
  DB.exists (setupFile name ctx) ctx >>= \exists ->
    if exists
      then printf "An entry with the name '%s' already exists on this shelf" name >> exitFailure
      else file ctx >>= \f -> DB.insert f ctx
  where
    file = DB.makeFile name target
parseCommand (Remove name no_confirm) ctx = checkFileExists name ctx >> printf "Test" >> getConfirm >>= removeDecider
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
parseCommand (MoveCmd from to name) ctx = parseCommand (CopyCmd from to name) ctx >> DB.removeFile name (DB.changeTargetShelf (DB.ShelfName from) ctx)
parseCommand (CopyCmd from to name) ctx =
  if from == to
    then die "Source and destination shelves must be different"
    else existsCheck >> ((\src -> doCopy (DB.ShelfName to) (DB.changeTargetShelf src ctx)) $ DB.ShelfName from)
  where
    doCopy = \to to_ctx -> checkFileNotExists name to_ctx >> DB.copyEntryTo to name to_ctx
    existsCheck = checkShelfExists from ctx >> checkShelfExists to ctx >> checkFileExists name ctx
parseCommand (RenameCmd from to) ctx = existsCheck >> DB.getFiles from ctx >>= \files -> DB.rename (files !! 0) to ctx
  where
    existsCheck = checkFileExists from ctx
parseCommand (VersionCmd) _ = Pretty.printVersionInfo

parseShelvesCmd :: ShelfArgs -> DB.Context -> IO ()
parseShelvesCmd (AddShelf name) ctx = checkShelfNotExists name ctx >> DB.insert (DB.ShelfName name) ctx
parseShelvesCmd (RemoveShelf name no_confirm) ctx = checkShelfExists name ctx >> getConfirm >>= removeDecider
  where
    getConfirm =
      if no_confirm
        then return True
        else getConfirmationYesNo "This will permanently delete this shelf and all its entries"
    removeDecider remove =
      if remove
        then DB.removeShelf name ctx
        else return ()
parseShelvesCmd (ListShelves) ctx = DB.getAllShelfNames ctx >>= Pretty.printList
parseShelvesCmd (RenameShelf from to) ctx = existsCheck >> DB.rename (DB.ShelfName from) to ctx
  where
    existsCheck = checkShelfExists from ctx

extractPaths :: [DB.File] -> [Text]
extractPaths files = map (\f -> DB.path f) files

pathsPrinter :: Bool -> [DB.File] -> IO ()
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

setupFile :: Text -> DB.Context -> DB.File
setupFile name ctx = DB.File name "" (DB.target_shelf ctx)

checkShelfExists :: Text -> DB.Context -> IO ()
checkShelfExists name ctx =
  DB.exists (DB.ShelfName name) ctx
    >>= \exists ->
      if exists
        then return ()
        else printf "Shelf '%s' does not exist" name >> exitFailure

checkFileExists :: Text -> DB.Context -> IO ()
checkFileExists name ctx =
  DB.exists (setupFile name ctx) ctx
    >>= \exists ->
      if exists
        then return ()
        else printf "Entry '%s' does not exist (are you on the right shelf?)" name >> exitFailure

checkFileNotExists :: Text -> DB.Context -> IO ()
checkFileNotExists name ctx =
  DB.exists (setupFile name ctx) ctx
    >>= \exists ->
      if exists
        then printf "Entry '%s' already exists on that shelf" name >> exitFailure
        else return ()

checkShelfNotExists :: Text -> DB.Context -> IO ()
checkShelfNotExists name ctx =
  DB.exists (DB.ShelfName name) ctx
    >>= \exists ->
      if exists
        then printf "Shelf '%s' already exists" name >> exitFailure
        else return ()
