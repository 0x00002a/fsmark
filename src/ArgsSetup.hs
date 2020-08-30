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
    (<|>),
  )
import qualified Options.Applicative as O

data ArgsResult
  = ArgsResult Command (Maybe Text) (Maybe Text) Bool

data ShelfArgs
  = AddShelf Text
  | RemoveShelf Text Bool
  | ListShelves
  | RenameShelf Text Text
  | ShelvesHelp Command
  | ExportShelf Text (Maybe FilePath)
  | ImportShelf (Maybe FilePath)

data Command
  = AddCmd [FilePath] (Maybe Text) Bool Bool Integer
  | List Bool (Maybe Text)
  | Remove Text Bool
  | ShelfCmd ShelfArgs
  | CopyCmd Text Text Text
  | MoveCmd Text Text Text
  | RenameCmd Text Text
  | VersionCmd
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
    cmdInfo (SubParserGroup name) = O.commandGroup $ unpack name

generateDescSubParser sections = generateSection $ SubParser $ map makeDesc sections
  where
    makeDesc :: (Text, Text, Maybe (O.Parser a)) -> SubParserDesc a
    makeDesc (name, "", Nothing) = SubParserGroup name
    makeDesc (name, desc, (Just opts)) = SubParserDesc name desc opts

shelfCommands =
  ShelfCmd
    <$> generateDescSubParser
      [ ("add", "Add an new shelf", Just shelfAddOpts),
        ("remove", "Remove a shelf", Just shelfRemoveOpts),
        ("list", "List all shelves", Just $ pure ListShelves),
        ("rename", "Rename a shelf", Just renameShelfOpts),
        ("import", "Import a shelf from a file or stdin", Just $ ImportShelf <$> optionalFilePathArg),
        ("export", "Export a shelf to a file or stdout", Just exportOpts)
      ]
  where
    shelfAddOpts =
      AddShelf <$> strArgument (metavar "NAME")
    shelfRemoveOpts =
      RemoveShelf <$> strArgument (metavar "NAME") <*> noConfirmSwitch
    renameShelfOpts = fromToOpts RenameShelf
    exportOpts = ExportShelf <$> strArgument (metavar "SHELF" <> help "Name of the shelf to be exported") <*> optionalFilePathArg

mainCommands =
  generateDescSubParser
    [ ("add", "Add an entry to the current shelf", Just add_opts),
      ("list", "List entries on the current shelf", Just list_opts),
      ("remove", "Remove an entry from the current shelf", Just remove_opts),
      ("move", "Move an entry to a different shelf", Just moveEntryOpts),
      ("copy", "Copy an entry to a different shelf", Just copyEntryOpts),
      ("rename", "Rename an entry", Just renameEntryOpts),
      ("fp", "Print path for entry with exact NAME", Just pathPrintOpts)
    ]
    <|> generateDescSubParser
      [ ("Shelf commands", "", Nothing),
        ("shelves", "Operate on shelves", Just shelfCommands)
      ]
    <|> generateDescSubParser
      [ ("Misc commands", "", Nothing),
        ("version", "Print version information", Just $ pure VersionCmd),
        ("license", "Print license information", Just $ pure ViewLicenseCmd)
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
        <$> O.many (argument str (metavar "PATH"))
        <*> ( optional $
                strOption
                  ( long "name"
                      <> short 'n'
                      <> help "Set a name for the target (removes name confirmation dialog). Does nothing if multiple paths are specified"
                  )
            )
        <*> noConfirmSwitch
        <*> switch
          ( long "recursive"
              <> short 'r'
              <> help "Add recursively"
          )
        <*> O.option
          O.auto
          ( long "depth"
              <> short 'd'
              <> help "Recursion depth, no effect without -r|--recursive. Set to -1 for maximum possible"
              <> O.value 1
          )

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
        <*> switch
          ( long "dry-run"
              <> help "Act as if doing things but don't actually write anything"
          )

    internalShelfArg =
      strOption
        ( long "shelf"
            <> short 's'
            <> help "The shelf to use"
        )

noConfirmSwitch = switch (long "no-confirm" <> short 'y' <> help "Auto accept all confirmation dialogs")

fromToOpts target = target <$> fromArg <*> toArg
  where
    fromArg = strOption (long "from" <> short 'f' <> help "Source name" <> metavar "SOURCE")
    toArg = strOption (long "to" <> short 't' <> help "Target name" <> metavar "TARGET")

filePathArg = strArgument (metavar "FILE")

optionalFilePathArg = optional filePathArg
