import Web.Scotty

import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))

import FileUtils (uploadLocation, uniqueAssetName, localPath)
import ServerUtils (Config(..), getConfig, getCorsPolicy)
import MediaConversion (convertImage)

import Data.Aeson (object, (.=))
import Data.Text.Lazy (unpack, pack, splitOn)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (makeRelative)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import Network.HTTP.Types.Status (badRequest400, created201, notFound404)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors (cors)
import Network.Wai.Parse (FileInfo(..))

import Network.URL (importURL)

main :: IO ()
main = do
  createDirectoryIfMissing False uploadLocation
  either putStrLn startApp =<< getConfig

jsonError :: String -> ActionM ()
jsonError err = json $ object ["error" .= err]

startApp :: Config -> IO ()
startApp cfg =
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
          setHeader "Location" $ pack uniqueName
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
          liftIO $ convertImage cmds path output

          status created201
          setHeader "Location" $ pack output
        else do
          status notFound404
          jsonError $ "image not found for conversion: " ++ url

    -- post "/compress" $ do
    --   command <- param "command"
    --   url <- param "url"
    --   res <- liftIO $ compressVideos command (removeHost cfg url)
    --   succeedWithLocation res
