import Web.Scotty

import Control.Monad.IO.Class
import Control.Applicative((<$>))
import Control.Concurrent(forkIO)
import Data.Traversable(traverse)
import Data.List(intercalate)

import qualified FileUtils as FU
import ServerUtils
import MediaConversion

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse
import Network.Wai (Request(..), requestHeaders)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as T

import System.FilePath((</>), takeExtension)
import System.Random(newStdGen, randomRs)
import System.Directory(createDirectoryIfMissing)
import System.Process(readProcessWithExitCode, readProcess)
import Data.List.Split(splitOn)
import Data.Map.Strict(Map, (!), fromList)

-- import System.Environment hiding (getEnv)
import Network.JobQueue hiding (param)

fileToTuple :: forall t t1. (t, FileInfo t1) -> (String, t1)
fileToTuple (_, fi) = (BS.unpack (fileName fi), fileContent fi)

compressVideos :: String -> FilePath -> IO FilePath
compressVideos command input_file = do
  mapM_ (compressVideo input_file) . splitOn "," $ command
  return input_file

saveFile :: (FilePath, B.ByteString) -> IO FilePath
saveFile (fn, fc) = do
  folder <- fmap (</>fn) generateFolder
  _ <- B.writeFile folder fc
  if isVideo fn then getScreenshot folder else return folder

startApp :: Config -> IO ()
startApp cfg =
  scotty (port cfg) $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase FU.uploadLocation)
    middleware $ cors getCorsPolicy

    post "/upload" $ do
      xs <- traverse (liftIO . saveFile . fileToTuple) =<< files
      let res = T.pack . intercalate "," . map (addHost cfg) $ xs
      setHeader "Location" res
      json $ object ["success" .= True, "url" .= res ]

    post "/crop" $ do
      command <- splitOn "," $ param "command"
      url <- param "url"
      res <- liftIO $ cropImage command (removeHost cfg url)
      succeedWithLocation res

    post "/compress" $ do
      command <- param "command"
      url <- param "url"
      res <- liftIO $ compressVideos command (removeHost cfg url)
      succeedWithLocation res

  where
    succeedWithLocation path = do
      let newUrl = addHost cfg path
      setHeader "Location" $ T.pack newUrl
      json $ object ["success" .= True, "url" .= newUrl ]

main :: IO()
main = either putStrLn startApp =<< getConfig
