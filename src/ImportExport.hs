module ImportExport
  ( Exportable (formatForExport),
  )
where

import qualified DB
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Json as J
import System.IO (Handle)
import qualified Types as T

class Exportable a where
  formatForExport :: a -> DB.Context -> IO ByteString

instance Exportable T.Shelf where
  formatForExport = J.shelfToJson

exportToFile :: (Exportable a) => a -> DB.Context -> FilePath -> IO ()
exportToFile target ctx path = formatForExport target ctx >>= B.writeFile path

exportToHandle :: (Exportable a) => a -> DB.Context -> Handle -> IO ()
exportToHandle target ctx handle = formatForExport target ctx >>= B.hPut handle
