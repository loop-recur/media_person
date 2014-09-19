module FileUtils where

import Data.Maybe (listToMaybe)
import System.IO (openTempFile, hClose)
import System.FilePath (makeRelative, (</>))
import Network.URL (URL(..), Host(..), URLType(..))
import Text.Regex.TDFA ((=~))
import Safe (atMay)

uploadLocation :: FilePath
uploadLocation = "uploads"

-- | Unique name for upload that does not conflict with existing files
--
-- > uniqueUploadName "foo.bar" == "foo123.bar"
uniqueUploadName :: String -> IO FilePath
uniqueUploadName originalName = do
  (path, handle) <- openTempFile uploadLocation originalName
  hClose handle
  return path

-- | The public url at which an upload is accessible
--
-- > publicUrl host "uploads/foo.bar" == "http://me.com/foo.bar"
publicUrl :: Host -> FilePath -> URL
publicUrl h file = URL (Absolute h) (makeRelative uploadLocation file) []

-- | The file path to retrieve an upload
--
-- > publicUrl "http://me.com/foo.bar" == "uploads/foo.bar"
localPath :: URL -> Maybe FilePath
localPath url = do
  let locationRegex = uploadLocation ++ "/(.+)"
  matches <- listToMaybe (url_path url =~ locationRegex :: [[FilePath]])
  filename <- atMay matches 1  -- [_, filename]
  return $ uploadLocation </> filename
