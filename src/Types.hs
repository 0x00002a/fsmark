module Types
  ( Entry (Entry, name, path),
    Shelf (ShelfID, ShelfName),
    Types.FilePath (..),
  )
where

import Data.Text (Text, unpack)

newtype FilePath = FP Text

data Entry = Entry
  { name :: Text,
    path :: Text
  }
  deriving (Ord)

data Shelf
  = ShelfID Integer
  | ShelfName Text
  deriving (Show, Eq)

instance Show Entry where
  show f = "Name: " ++ unpack (name f) ++ "\nPath: " ++ unpack (path f)

instance Eq Entry where
  (==) lhs rhs = name lhs == name rhs
