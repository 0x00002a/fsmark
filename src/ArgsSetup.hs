-- Copyright (C) 2020 Natasha England-Elbro
--
-- This file is part of fsmark.
--
-- fsmark is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- fsmark is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with fsmark.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE OverloadedStrings #-}

module ArgsSetup
  ( generateArgsInfo,
    ArgsResult (ArgsResult),
    ShelfArgs (..),
    TargetShelfArg (..),
    Command (..),
  )
where

import Data.Text (Text, append, pack, unpack)
import Options.Applicative
  ( argument,
    help,
    helper,
    long,
    many,
    metavar,
    optional,
    short,
    str,
    strArgument,
    strOption,
    subparser,
    switch,
    (<**>),
  )
import qualified Options.Applicative as O

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

data SubParserDesc a
  = SubParserDesc Text Text (O.Parser a)
  | SubParserGroup Text

data ParserSection a
  = SubParser [SubParserDesc a]
  | GlobalArg (O.Parser a)

generateSection :: ParserSection a -> O.Parser a
generateSection (SubParser descs) = (subparser . foldMap cmdInfo) descs
  where
    info' p desc = O.info (helper <*> p) (O.fullDesc <> O.progDesc desc)
    cmdInfo (SubParserDesc cmd_name desc parser) = O.command (unpack cmd_name) (info' parser (unpack desc))

generateDescSubParser sections = generateSection $ SubParser $ map makeDesc sections
  where
    makeDesc (name, desc, opts) = SubParserDesc name desc opts

shelfCommands =
  ShelfCmd
    <$> generateDescSubParser
      [ ("add", "Add an new shelf", shelfAddOpts),
        ("remove", "Remove a shelf", shelfRemoveOpts),
        ("list", "List all shelves", pure ListShelves),
        ("rename", "Rename a shelf", renameShelfOpts),
        ("import", "Import a shelf from a file or stdin", ImportShelf <$> optionalFilePathArg),
        ("export", "Export a shelf to a file or stdout", ExportShelf <$> optionalFilePathArg)
      ]
  where
    filePathArg = strArgument (metavar "FILE")
    optionalFilePathArg = optional filePathArg

    shelfAddOpts =
      AddShelf <$> strArgument (metavar "NAME")
    shelfRemoveOpts =
      RemoveShelf <$> strArgument (metavar "NAME") <*> noConfirmSwitch
    renameShelfOpts = fromToOpts RenameShelf

mainCommands =
  generateDescSubParser
    [ ("add", "Add an entry to the current shelf", add_opts),
      ("list", "List entries on the current shelf", list_opts),
      ("remove", "Remove an entry from the current shelf", remove_opts),
      ("shelves", "Operate on shelves", shelfCommands),
      ("move", "Move an entry to a different shelf", moveEntryOpts),
      ("copy", "Copy an entry to a different shelf", copyEntryOpts),
      ("rename", "Rename an entry", renameEntryOpts),
      ("fp", "Print entry with path, shorthand for: list --path --name <TARGET>", pathPrintOpts),
      ("version", "Print version information", pure VersionCmd),
      ("license", "Print license information", pure ViewLicenseCmd)
    ]
  where
    makeDesc (name, desc, opts) = SubParserDesc name desc opts
    pathPrintOpts =
      (\arg -> List False (Just arg)) <$> strArgument (metavar "TARGET")

    renameEntryOpts = fromToOpts RenameCmd

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

generateArgsInfo :: O.ParserInfo ArgsResult
generateArgsInfo = makeArgsInfo mainCommands

fsmDesc =
  O.progDesc "A bookmarking system for your filesystem"
    <> O.footer "'fsm <command> --help' can be used to view more specific help for each command"

makeArgsInfo :: O.Parser Command -> O.ParserInfo ArgsResult
makeArgsInfo cmd = O.info (args <**> helper) (O.fullDesc <> fsmDesc)
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

noConfirmSwitch = switch (long "no-confirm" <> short 'y' <> help "Auto accept all confirmation dialogs")

fromToOpts target = target <$> fromArg <*> toArg
  where
    fromArg = strOption (long "from" <> short 'f' <> help "Source name" <> metavar "SOURCE")
    toArg = strOption (long "to" <> short 't' <> help "Target name" <> metavar "TARGET")
