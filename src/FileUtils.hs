module FileUtils where

import System.IO (openTempFile, hClose)
import System.FilePath (makeRelative, (</>))
import Network.URL (URL(..), URLType(..))

uploadLocation :: FilePath
uploadLocation = "uploads"

-- | Unique name for upload that does not conflict with existing files
--
-- > uniqueUploadName "foo.bar" == "foo123.bar"
uniqueAssetName :: String -> IO FilePath
uniqueAssetName originalName = do
  (path, handle) <- openTempFile uploadLocation originalName
  hClose handle
  return path

-- | The public url at which an upload is accessible
--
-- > publicUrl host "uploads/foo.bar" == "/foo.bar"
publicUrl :: FilePath -> URL
publicUrl file = URL HostRelative (makeRelative uploadLocation file) []

-- | The file path to retrieve an upload
--
-- > publicUrl "http://me.com/foo.bar" == "uploads/foo.bar"
localPath :: URL -> FilePath
localPath = (uploadLocation </>) . url_path
