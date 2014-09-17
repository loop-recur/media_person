{-# LANGUAGE OverloadedStrings, RankNTypes, DeriveGeneric #-}
import Web.Scotty

import Control.Monad.IO.Class
import Control.Monad(join)
import Control.Applicative((<$>))
import Control.Concurrent(forkIO)
import Data.Traversable(traverse)
import Data.List(intercalate)

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors
import Network.Wai.Parse
import Network.Wai (Request(..), requestHeaders)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as T
import Data.Aeson(FromJSON,eitherDecode,object,(.=)) --JSON

import System.FilePath((</>), takeExtension)
import System.Random(newStdGen, randomRs)
import System.Directory(createDirectoryIfMissing)
import System.Process(readProcessWithExitCode, readProcess)
import Data.String.Utils(replace)
import Data.List.Split(splitOn)
import Data.Map.Strict(Map, (!), fromList)
import GHC.Generics

data Config = Config { host :: String, port :: Int } deriving (Show, Generic)
instance FromJSON Config

getCorsPolicy :: Request -> Maybe CorsResourcePolicy
getCorsPolicy req = case hdrOrigin of
    Just _ -> Just CorsResourcePolicy {
        corsMethods=["GET", "PUT", "POST"]
      , corsOrigins=Nothing
      , corsRequestHeaders=["x-requested-with", "content-type", "cache-control", "Authorization"]
      , corsExposedHeaders=Just ["Access-Control-Allow-Origin"]
      , corsMaxAge=Just 1000
      , corsVaryOrigin=True
      , corsRequireOrigin=False
      , corsIgnoreFailures=True
    }
    _ -> Nothing
  where
    hdrOrigin = lookup "origin" (requestHeaders req)

fileToTuple :: forall t t1. (t, FileInfo t1) -> (String, t1)
fileToTuple (_, fi) = (BS.unpack (fileName fi), fileContent fi)

presetMap :: Map String (String, String)
presetMap = fromList [("h264", ("-vcodec,libx264,-preset,fast,-crf,22", ".mp4")), ("ogg", ("-c:v,libtheora,-c:a,libvorbis,-q:v,10,-q:a,10", ".ogv"))]

getPresent :: String -> (String, String)
getPresent x = presetMap ! x

videoExtensions :: [String]
videoExtensions = [".avi", ".wmv", ".flv", ".mpg", ".mpeg", ".mp4", ".mov", ".m4v"]

insertFile :: FilePath -> String -> FilePath
insertFile path x = fn ++ x ++ ext
  where (fn, ext) = break (=='.') path

getPathName :: FilePath -> IO FilePath
getPathName path = (insertFile path . take 4 . randomRs ('a','z')) <$> newStdGen

getMovPathName :: FilePath -> String -> FilePath
getMovPathName path x = replace ext x path
  where (_, ext) = break (=='.') path

makeArgs :: FilePath -> FilePath -> String -> [String]
makeArgs input_file output_file command = [input_file]++cmds++[output_file]
  where cmds = splitOn "," command

cropImage :: String -> FilePath -> IO FilePath
cropImage command input_file = do
  output_file <- getPathName input_file
  _ <- readProcessWithExitCode "convert" (makeArgs input_file output_file command) ""
  return output_file

getScreenshot :: FilePath -> IO FilePath
getScreenshot input_file = do
  let output_file = getMovPathName input_file ".jpg"
  let args = makeArgs "-i" output_file (input_file++",-vframes,1,-f,image2,-an")
  _ <- readProcessWithExitCode "ffmpeg" args ""
  return output_file

compressVideo :: FilePath -> String -> IO FilePath
compressVideo input_file format = do
  let (cmd, ext) = getPresent format
  let output_file = getMovPathName input_file ext
  _ <- forkIO $ mapM_ (\x -> readProcess "ffmpeg" x "") [makeArgs "-i" output_file (input_file++","++cmd)]
  return input_file

compressVideos :: String -> FilePath -> IO FilePath
compressVideos command input_file = do
  mapM_ (compressVideo input_file) . splitOn "," $ command
  return input_file

generateFolder :: IO FilePath
generateFolder = do
  folder <- fmap (("uploads"</>) . take 10 . randomRs ('a','z')) newStdGen
  createDirectoryIfMissing True folder
  return folder

isVideo :: FilePath -> Bool
isVideo = flip elem videoExtensions . takeExtension

saveFile :: (FilePath, B.ByteString) -> IO FilePath
saveFile (fn, fc) = do
  folder <- fmap (</>fn) generateFolder
  _ <- B.writeFile folder fc
  if isVideo fn then getScreenshot folder else return folder

addHost :: Config -> String -> String
addHost cfg x = host cfg ++ ":" ++ (show.port $ cfg) ++ replace "uploads" "" x

removeHost :: Config -> String -> String
removeHost cfg = replace (host cfg ++ ":" ++ (show.port $ cfg)) "uploads"

getConfig :: IO (Either String Config)
getConfig = eitherDecode <$> B.readFile "config.json"

-- checkKey :: String -> Request -> Bool
-- checkKey k req = maybe False (== k) reqKey
--   where reqKey = fmap BS.unpack $ lookup "Authorization" (requestHeaders req)

startApp :: Config -> IO ()
startApp cfg =
  scotty (port cfg) $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "uploads")
    middleware $ cors getCorsPolicy

    post "/upload" $ do
      xs <- traverse (liftIO . saveFile . fileToTuple) =<< files
      let res = T.pack . intercalate "," . map (addHost cfg) $ xs
      setHeader "Location" res
      json $ object ["success" .= True, "url" .= res ]

    post "/crop" $ do
      command <- param "command"
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
main = join . fmap (either putStrLn startApp) $ getConfig
