import Web.Scotty

-- imports {{{

import Paths_MediaPerson (version)

import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))

import FileUtils (uploadLocation, uniqueAssetName, localPath, publicUrl)
import ServerUtils (Config(..), getConfig, getCorsPolicy)
import MediaConversion (
  convertImage, conversions, compressVideo,
  convertedName, ConversionOpts(..))

import Data.Aeson (object, (.=))
import Data.Text.Lazy (splitOn, Text)
import Data.Maybe (fromMaybe, isJust)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (makeRelative, takeFileName, combine, splitExtension)
import qualified Data.ByteString.Lazy as BL
import Data.Monoid ( (<>) )

import Network.HTTP.Types.Status (badRequest400, ok200, created201, notFound404, badRequest400, accepted202)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors (cors)
import Network.Wai.Parse (FileInfo(..))

import Control.Concurrent (forkIO, threadDelay)

import Network.URL (importURL, exportURL)
import Network.JobQueue (buildJobQueue, process, executeJob, scheduleJob, Env, Aux, Desc, commitIO, JobQueue)
import Network.JobQueue.Job (Unit(..))
import qualified Network.JobQueue.Types as NJT

import qualified Network.Wai.Handler.Warp as Warp

import Data.String.Conversions (cs)
import Data.Version (Version(..))

import Text.Regex (subRegex, mkRegex)

--- }}}

data CompressionUnit = CompressionUnit {
  cuInput :: FilePath
, cuOutput :: FilePath
, cuConversion :: ConversionOpts
} deriving (Show, Read, Eq, Ord)

instance Desc CompressionUnit
instance Unit CompressionUnit

data JobEnv = JobEnv deriving (Eq, Show)

instance Env JobEnv where
instance Aux JobEnv where

prettyVersion :: String
prettyVersion = intercalate "." $ map show $ versionBranch version

main :: IO ()
main = do
  createDirectoryIfMissing False uploadLocation
  either putStrLn startApp =<< getConfig


jsonError :: String -> ActionM ()
jsonError err = json $ object ["error" .= err]


videoWorker :: (Env e, Unit a) => CompressionUnit -> NJT.ActionM e a ()
videoWorker job = commitIO $ do
  putStrLn $ "Running job " ++ show job
  compressVideo (cuConversion job) (cuInput job) (cuOutput job)


maybeParam :: Text -> ActionM (Maybe Text)
maybeParam p = M.lookup p . M.fromList <$> params

cleanFileName :: String -> String
cleanFileName = sanitizeName . splitExtension
  where
   sanitizeName (x,y) = (clean x) ++  y
   clean x = subRegex (mkRegex "\\W+") x "_"


startApp :: Config -> IO ()
startApp cfg = do
  let withVideoQueue :: (JobQueue JobEnv CompressionUnit -> IO ()) -> IO ()
      withVideoQueue = buildJobQueue
        "sqlite3://media_person.sqlite3" "compress" $ process videoWorker

  _ <- forkIO $ forever $ do
    threadDelay 2000000 -- 2 seconds
    withVideoQueue $ flip executeJob JobEnv


  let opts = Warp.setServerName (cs $ "MediaPerson/" <> prettyVersion)
               . Warp.setPort (port cfg)
               $ Warp.defaultSettings

  scottyOpts (Options 1 opts) $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase uploadLocation)
    middleware $ cors getCorsPolicy


    post "/uploads" $ do
      fs <- files

      case fs of
        [f] -> do
          let info = snd f
          let name = cleanFileName . cs . fileName $ info
          let content = fileContent info
          uniqueName <- liftIO $ uniqueAssetName name

          _ <- liftIO $ BL.writeFile uniqueName content
          status created201

          let public = exportURL (publicUrl uniqueName)
          setHeader "Location" $ cs public

          accept       <- header "Accept"
          iframeRemote <- maybeParam "iframeRemote"
          when (isJust iframeRemote ||
                accept == Just "text/vnd.fineuploader+plain") $ do
            status ok200
            json $ object [
                "success" .= True
              , "location" .= public
              , "contentType" .= ((cs $ fileContentType info) :: String)
              ]
            setHeader "Content-Type" "text/plain"

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
          cmds   <- map cs <$> splitOn "," <$> param "command"
          result <- liftIO $ convertImage cmds path output

          case result of
            Right _ -> do
              status created201
              setHeader "Location" $ cs $ exportURL (publicUrl output)
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
      overrideName <- maybeParam "targetFilename"
      let overridePath = combine uploadLocation . takeFileName . cs <$> overrideName

      let conversionOpt = M.lookup fmt conversions
      case conversionOpt of
        Just opt -> do
          present <- liftIO $ doesFileExist path
          if present
            then do
              let name = makeRelative uploadLocation path
              defaultName <- liftIO $ uniqueAssetName $ convertedName opt name
              let output = fromMaybe defaultName overridePath

              if convAsync opt
                then do
                  _ <- liftIO $ do
                    let job = CompressionUnit path output opt
                    putStrLn $ "Queuing video job " ++ show job
                    withVideoQueue $ flip scheduleJob job

                  status accepted202
                else do
                  liftIO $ compressVideo opt path output
                  status created201
                  prefer <- header "Prefer"
                  when (prefer == Just "return=representation") $ do
                    setHeader "Content-Type" "image/jpeg"
                    file output

              setHeader "Location" $ cs $ exportURL (publicUrl output)
            else do
              status notFound404
              jsonError $ "video not found for compression: " ++ url
        _ -> do
          status badRequest400
          jsonError $ "Unknown destination format " ++ fmt
