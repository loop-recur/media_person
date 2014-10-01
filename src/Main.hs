import Web.Scotty

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))

import FileUtils (uploadLocation, uniqueAssetName, localPath, publicUrl)
import ServerUtils (Config(..), getConfig, getCorsPolicy)
import MediaConversion
  (convertImage, conversions, compressVideo, convertedName,
   ConversionOpts(..))

import Data.Aeson (object, (.=))
import Data.Text.Lazy (unpack, pack, splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (makeRelative)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import Network.HTTP.Types.Status (badRequest400, created201, notFound404, badRequest400, accepted202)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors (cors)
import Network.Wai.Parse (FileInfo(..))

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TChan

import Network.URL (importURL, exportURL)

main :: IO ()
main = do
  createDirectoryIfMissing False uploadLocation
  either putStrLn startApp =<< getConfig

jsonError :: String -> ActionM ()
jsonError err = json $ object ["error" .= err]

data VideoJob = VideoJob {
  vjInput :: FilePath
, vjOutput :: FilePath
, vjConversion :: ConversionOpts
} deriving Show

videoWorker :: TChan VideoJob -> IO ()
videoWorker c = forever $ do
  job <- atomically $ readTChan c
  putStrLn $ "Running job " ++ show job
  compressVideo (vjConversion job) (vjInput job) (vjOutput job)


startApp :: Config -> IO ()
startApp cfg = do
  videoQueue <- liftIO $ atomically (newTChan :: STM (TChan VideoJob))
  _ <- forkIO $ videoWorker videoQueue

  scotty (port cfg) $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase uploadLocation)
    middleware $ cors getCorsPolicy


    post "/uploads" $ do
      fs <- files

      case fs of
        [f] -> do
          let info = snd f
          let name = BS.unpack $ fileName info
          let content = fileContent info
          uniqueName <- liftIO $ uniqueAssetName name

          _ <- liftIO $ BL.writeFile uniqueName content
          status created201
          setHeader "Location" $ pack $ exportURL (publicUrl uniqueName)
        _ -> do
          status badRequest400
          jsonError "requires exactly one file in post"


    post "/conversions" $ do
      url      <- param "url"
      let path = fromMaybe "" $ localPath <$> importURL url

      present <- liftIO $ doesFileExist path
      if present
        then do
          let name = makeRelative uploadLocation path
          output <- liftIO $ uniqueAssetName name
          cmds   <- map unpack <$> splitOn "," <$> param "command"
          result <- liftIO $ convertImage cmds path output

          case result of
            Right _ -> do
              status created201
              setHeader "Location" $ pack $ exportURL (publicUrl output)
            Left message -> do
              status badRequest400
              jsonError message
        else do
          status notFound404
          jsonError $ "image not found for conversion: " ++ url


    post "/compressions" $ do
      url <- param "url"
      fmt <- param "targetFormat"
      let path = fromMaybe "" $ localPath <$> importURL url

      let conversionOpt = M.lookup fmt conversions
      case conversionOpt of
        Just opt -> do
          present <- liftIO $ doesFileExist path
          if present
            then do
              let name = makeRelative uploadLocation path
              output <- liftIO $
                uniqueAssetName $ convertedName opt name
              let job = VideoJob path output opt
              liftIO $ do
                putStrLn $ "Queuing video job " ++ show job
                atomically $ writeTChan videoQueue job
              status accepted202
              setHeader "Location" $ pack $ exportURL (publicUrl output)
            else do
              status notFound404
              jsonError $ "video not found for compression: " ++ url
        _ -> do
          status badRequest400
          jsonError $ "Unknown destination format " ++ fmt
