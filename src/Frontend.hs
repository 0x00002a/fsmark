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
import Control.Exception
  ( throw,
    try,
  )
import Control.Monad
  ( unless,
    when,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified DB
import Data.Maybe
  ( catMaybes,
    mapMaybe,
  )
import Data.Text
  ( Text,
    append,
    pack,
    unpack,
  )
import qualified Exceptions as EX
import qualified FSM
import Options.Applicative
import qualified Pretty
import System.IO
  ( IOMode (..),
    hFlush,
    hPutStr,
    stdout,
  )
import Text.Printf (printf)
import qualified Types as T

runFSM :: IO ()
runFSM =
  customExecParser p generateArgsInfo
    >>= \options -> try (parseOptions options) >>= handleExceptions
  where
    handleExceptions :: Either EX.Error () -> IO ()
    handleExceptions (Left exception) = print exception
    handleExceptions (Right _) = return ()
    p = prefs (showHelpOnEmpty <> disambiguate)

handleErrors :: Either EX.Error a -> IO ()
handleErrors err = case err of
  Left err -> EX.printError err
  Right _ -> return ()

defaultShelfName :: Text
defaultShelfName = "default"

defaultShelf :: T.Shelf
defaultShelf = T.Shelf {T.s_name = defaultShelfName, T.s_entries = []}

parseOptions :: ArgsResult -> IO ()
parseOptions (ArgsResult cmd tshelf) =
  checkWhetherHandled $ preparseCmd cmd
  where
    checkWhetherHandled Nothing =
      parseTargetShelf tshelf >>= \s -> case s of
        Just shelf -> parseCommand cmd shelf
        Nothing -> return ()
    checkWhetherHandled (Just s) = s

    preparseCmd VersionCmd = Just $ Pretty.printVersionInfo
    preparseCmd ViewLicenseCmd = Just $ Pretty.printLicense
    preparseCmd _ = Nothing

    parseTargetShelf :: (Maybe Text) -> IO (Maybe T.Shelf)
    parseTargetShelf Nothing = ensureExistsDefault
    parseTargetShelf (Just name) = DB.loadShelfFromName name >>= \s -> checkShelf s >> return s
      where
        checkShelf Nothing = throw $ EX.DoesNotExist $ EX.Shelf name
        checkShelf (Just _) = return ()

    ensureExistsDefault :: IO (Maybe T.Shelf)
    ensureExistsDefault =
      DB.shelfExists defaultShelfName >>= \ex ->
        if ex
          then DB.loadShelfFromName defaultShelfName
          else DB.saveShelfDefault defaultShelf >> (Just <$> return defaultShelf)

parseCommand :: Command -> T.Shelf -> IO ()
parseCommand (List path_only match) shelf = printItems $ FSM.entriesMatching shelf match
  where
    printItems [] = case match of
      Just m ->
        throw $ EX.TextError $ "No items matching '" `append` m `append` "'"
      Nothing -> return ()
    printItems items = pathsPrinter path_only items
parseCommand (AddCmd path chosen_name no_confirm) shelf =
  getEntry >>= saveEnt
  where
    saveEnt :: Maybe T.Entry -> IO ()
    saveEnt Nothing = return ()
    saveEnt (Just ent) = DB.saveShelf (FSM.addEntry ent shelf) Nothing
    getEntry :: IO (Maybe T.Entry)
    getEntry = case chosen_name of
      Just name -> Just <$> FSM.makeEntry name path
      Nothing -> createEntryFromInput path no_confirm
parseCommand (Remove name no_confirm) shelf = (removeDecider <$> getConfirm) >>= DB.saveShelfDefault
  where
    getConfirm =
      if no_confirm
        then return True
        else
          getConfirmationYesNo
            "This will permanently remove this entry, are you sure?"
    removeDecider :: Bool -> T.Shelf
    removeDecider True = FSM.removeEntryByName shelf name
    removeDecider False = shelf
parseCommand (ShelfCmd cmd) _ = parseShelvesCmd cmd
parseCommand (MoveCmd from to name) shelf = FSM.moveEntryByName from to name >> return ()
parseCommand (CopyCmd from to name) shelf =
  FSM.copyEntryByName' (from) (to) name >> return ()
parseCommand (RenameCmd from to) shelf = DB.saveShelfDefault $ FSM.renameEntryByName from to shelf

parseShelvesCmd :: ShelfArgs -> IO ()
parseShelvesCmd (AddShelf name) = DB.saveShelfDefault (T.Shelf {T.s_name = name, T.s_entries = []})
parseShelvesCmd (RemoveShelf name no_confirm) =
  getConfirm >>= removeDecider
  where
    getConfirm =
      if no_confirm
        then return True
        else
          getConfirmationYesNo
            "This will permanently remove this shelf and all of it's entries, are you sure?"
    removeDecider remove = when remove $ DB.removeShelf name
parseShelvesCmd ListShelves = (catMaybes <$> DB.allShelves) >>= Pretty.printList
parseShelvesCmd (RenameShelf from to) = FSM.renameShelfByName from to
parseShelvesCmd (ImportShelf path) = FSM.importShelf path >> return ()
parseShelvesCmd (ExportShelf name path) = DB.loadShelfFromName name >>= check >>= FSM.exportShelf path >> return ()
  where
    check (Just name) = return name
    check Nothing = throw $ EX.DoesNotExist $ EX.Shelf name

extractPaths :: [T.Entry] -> [Text]
extractPaths = map T.path

pathsPrinter :: Bool -> [T.Entry] -> IO ()
pathsPrinter False files = mapM_ Pretty.printPath $ extractPaths files
pathsPrinter True files = Pretty.printList files

getConfirmationYesNo :: Text -> IO Bool
getConfirmationYesNo prompt =
  promptInput (prompt `append` "[y/n]: ") >>= checkLine
  where
    checkLine "yes" = return True
    checkLine "y" = return True
    checkLine "no" = return False
    checkLine "n" = return False
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
    nameAutoComplete Nothing = ": "
    decodeName "" (Just name) = return name
    decodeName "" Nothing =
      putStrLn "Please enter a valid name" >> namePrompt Nothing
    decodeName name _ = return name

createEntryFromInput :: FilePath -> Bool -> IO (Maybe T.Entry)
createEntryFromInput path confirm =
  act <$> (FSM.nameForPath path >>= check)
  where
    check :: Text -> IO (Maybe Text)
    check name =
      if confirm
        then
          getConfirmationYesNo name >>= \yn ->
            if yn
              then return $ Just name
              else return Nothing
        else return (Just name)
    act :: Maybe Text -> Maybe T.Entry
    act rs = (\n -> T.Entry n (pack path)) <$> rs
