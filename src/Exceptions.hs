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

module Exceptions where

import Control.Monad.Except (ExceptT)
import Data.Text (Text, append, unpack)
import Text.Printf (printf)
import qualified Types as T

data InfoType = Shelf Text | Entry Text

data Error
  = BadInput Text
  | DBError Text
  | DoesNotExist InfoType
  | NamingConflict InfoType
  | NotUnique Text

type Exception = ExceptT Error

printError :: Error -> IO ()
printError (BadInput msg) = putStrLn $ unpack msg
printError (DBError msg) = printf "Database error: %s\n" msg
printError (DoesNotExist e) = printf "%s does not exist\n" $ infoTMsg e
printError (NamingConflict e) = printf "%s already exists\n" $ infoTMsg e

infoTMsg (Shelf name) = "Shelf '" `append` name `append` "'"
infoTMsg (Entry name) = "Entry '" `append` name `append` "'"
