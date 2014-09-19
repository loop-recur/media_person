import Web.Scotty

import Control.Applicative((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (zipWithM)

import FileUtils (uploadLocation, uniqueUploadName)
import ServerUtils (Config(..), getConfig, getCorsPolicy)
--import MediaConversion

import Data.Aeson (object, (.=))
--import Network.JobQueue hiding (param)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors (cors)
import Network.Wai.Parse (FileInfo(..))


main :: IO()
main = either putStrLn startApp =<< getConfig

startApp :: Config -> IO ()
startApp cfg =
  scotty (port cfg) $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase uploadLocation)
    middleware $ cors getCorsPolicy

    post "/upload" $ do
      fileInfos <- map snd <$> files
      let fileNames = map (BS.unpack . fileName) fileInfos
      let fileContents = map fileContent fileInfos
      uniqueNames <- liftIO . sequence $ map uniqueUploadName fileNames

      _ <- liftIO $ zipWithM BL.writeFile uniqueNames fileContents

      json $ object ["success" .= True, "url" .= uploadLocation ]

      -- xs <- traverse (liftIO . saveFile . fileToTuple) =<< files
      -- let res = T.pack . intercalate "," . map (addHost cfg) $ xs
      -- setHeader "Location" res

    -- post "/crop" $ do
    --   command <- splitOn "," $ param "command"
    --   url <- param "url"
    --   res <- liftIO $ cropImage command (removeHost cfg url)
    --   succeedWithLocation res

    -- post "/compress" $ do
    --   command <- param "command"
    --   url <- param "url"
    --   res <- liftIO $ compressVideos command (removeHost cfg url)
    --   succeedWithLocation res

  -- where
    -- succeedWithLocation path = do
    --   let newUrl = addHost cfg path
    --   setHeader "Location" $ T.pack newUrl
    --   json $ object ["success" .= True, "url" .= newUrl ]

-- fileToTuple :: forall t t1. (t, FileInfo t1) -> (String, t1)
-- fileToTuple (_, fi) = (BS.unpack (fileName fi), fileContent fi)

-- compressVideos :: String -> FilePath -> IO FilePath
-- compressVideos command input_file = do
  -- mapM_ (compressVideo input_file) . splitOn "," $ command
  -- return input_file

-- saveFile :: (FilePath, B.ByteString) -> IO FilePath
-- saveFile (fn, fc) = do
  -- folder <- fmap (</>fn) generateFolder
  -- _ <- B.writeFile folder fc
  -- if isVideo fn then getScreenshot folder else return folder
