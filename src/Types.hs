module Types
  ( Entry (Entry, name, path, shelf_id),
    Shelf (ShelfID, ShelfName),
  )
where

import Data.Text (Text)

data Entry = Entry
  { name :: Text,
    path :: Text,
    shelf_id :: Shelf
  }

data Shelf
  = ShelfID Integer
  | ShelfName Text
  deriving (Show, Eq)
