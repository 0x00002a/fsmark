module Types
  ( Entry (Entry, name, path, shelf_id),
    Shelf (ShelfID, ShelfName),
    Types.FilePath (..),
  )
where

import Data.Text (Text, unpack)

newtype FilePath = FP Text

data Entry = Entry
  { name :: Text,
    path :: Text,
    shelf_id :: Shelf
  }

data Shelf
  = ShelfID Integer
  | ShelfName Text
  deriving (Show, Eq)

instance Show Entry where
  show f = "Name: " ++ unpack (name f) ++ "\nPath: " ++ unpack (path f)
