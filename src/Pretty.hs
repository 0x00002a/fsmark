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

module Pretty
    ( PrettyPrintable(..)
    , printList
    , printLicense
    , printPath
    , printVersionInfo
    )
where

import           Data.Text                      ( Text
                                                , append
                                                , pack
                                                , unpack
                                                )
import           Text.Printf                    ( printf )
import qualified Types                         as T
import qualified Sys

class PrettyPrintable a where
  display :: a -> IO ()

instance PrettyPrintable Text where
    display txt = putStrLn $ unpack txt

instance Pretty.PrettyPrintable T.Entry where
    display f = printf "Name: %s\nPath: %s\n\n"
                       (T.name f)
                       (Sys.escapePath (unpack (T.path f)) Sys.os)

instance Pretty.PrettyPrintable T.Shelf where
    display (T.ShelfName name) = display $ "Name: " `append` name
    display (T.ShelfID   id  ) = display $ "ID: " `append` pack (show id)

printList :: (PrettyPrintable a) => [a] -> IO ()
printList = mapM_ display

versionString :: Text
versionString = "0.5.1"

printVersionInfo :: IO ()
printVersionInfo = printf "fsm (version %s)\n" versionString

licenseStr =
    [ "fsm - A bookmarker for your filesystem"
    , "Copyright (C) 2020  Natasha England-Elbro "
    , ""
    , "This program is free software: you can redistribute it and/or modify "
    , "it under the terms of the GNU General Public License as published by "
    , "the Free Software Foundation, either version 3 of the License, or "
    , "(at your option) any later version. "
    , ""
    , "This program is distributed in the hope that it will be useful, "
    , "but WITHOUT ANY WARRANTY; without even the implied warranty of "
    , "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the "
    , "GNU General Public License for more details. "
    , ""
    , "You should have received a copy of the GNU General Public License "
    , "along with this program.  If not, see <https://www.gnu.org/licenses/>."
    ]

printLicense :: IO ()
printLicense = mapM_ putStrLn licenseStr


printPath :: Text -> IO ()
printPath path = putStrLn $ unpack path


