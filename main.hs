{-# LANGUAGE OverloadedStrings, RankNTypes, DeriveGeneric #-}
import Web.Scotty

import Control.Monad.IO.Class
import Control.Monad(join)
import Control.Applicative((<$>))
import Control.Concurrent(forkIO)
import Data.Traversable(traverse)
import Data.List(intercalate)

import Network.Wai (Application, Request(..), Response, responseLBS)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors
import Network.Wai.Parse
import Network.HTTP.Types (status401, methodOptions)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as T
import Data.Aeson(FromJSON,eitherDecode,object,(.=)) --JSON

import System.FilePath((</>))
import System.Random(newStdGen, randomRs)
import System.Directory(createDirectoryIfMissing)
import System.Process(readProcessWithExitCode, readProcess)
import Data.String.Utils(replace)
import Data.List.Split(splitOn)
import Data.Map.Strict(Map, (!), fromList)
import GHC.Generics

data Config = Config { host :: String, port :: Int, key :: String } deriving (Show, Generic)
instance FromJSON Config

getCorsPolicy :: a -> Maybe CorsResourcePolicy
getCorsPolicy = const $ Just (CorsResourcePolicy { corsMethods=["GET", "PUT", "POST"], corsOrigins=Nothing, corsRequestHeaders=["x-requested-with", "content-type", "cache-control", "Authorization"], corsExposedHeaders=(Just ["Access-Control-Allow-Origin"]), corsMaxAge=(Just 1000), corsVaryOrigin=True, corsVerboseResponse=True  })

fileToTuple :: forall t t1. (t, FileInfo t1) -> ([Char], t1)
fileToTuple (_, fi) = (BS.unpack (fileName fi), fileContent fi)

preset_map :: Map String (String, String)
preset_map = fromList [("h264", ("-vcodec,libx264,-preset,fast,-crf,22", ".mp4")), ("ogg", ("-c:v,libtheora,-c:a,libvorbis,-q:v,10,-q:a,10", ".ogv"))]

getPresent :: String -> (String, String)
getPresent x = preset_map ! x

insertFile :: FilePath -> String -> FilePath
insertFile path x = fn ++ x ++ ext
  where (fn, ext) = break (=='.') $ path

getPathName :: FilePath -> IO FilePath
getPathName path = fmap ((insertFile path) . take 4 . randomRs ('a','z')) $ newStdGen

getMovPathName :: FilePath -> String -> FilePath
getMovPathName path x = replace ext x path
  where (_, ext) = break (=='.') $ path

makeArgs :: FilePath -> FilePath -> String -> [String]
makeArgs input_file output_file command = ([input_file]++cmds++[output_file])
  where cmds = splitOn "," command

cropImage :: String -> FilePath -> IO FilePath
cropImage command input_file = do
  output_file <- getPathName input_file
  _ <- readProcessWithExitCode "convert" (makeArgs input_file output_file command) ""
  return output_file

getScreenshot :: FilePath -> IO FilePath
getScreenshot input_file = do
  let output_file = getMovPathName input_file ".jpg"
  let args = (makeArgs "-i" output_file (input_file++",-vframes,1,-f,image2,-an"))
  _ <- readProcessWithExitCode "ffmpeg" args ""
  return output_file

compressVideo :: FilePath -> String -> IO FilePath
compressVideo input_file format = do
  let (cmd, ext) = getPresent format
  let output_file = getMovPathName input_file ext
  _ <- forkIO $ mapM_ (\x -> readProcess "ffmpeg" x "") $ [makeArgs "-i" output_file (input_file++","++cmd)]
  return input_file

compressVideos :: String -> FilePath -> IO FilePath
compressVideos command input_file = do
  screenshot <- getScreenshot input_file
  _ <- ((mapM_ (compressVideo input_file)) . splitOn "," $ command)
  return screenshot

generateFolder :: IO FilePath
generateFolder = do
  folder <- fmap (("uploads"</>) . take 10 . randomRs ('a','z')) newStdGen
  createDirectoryIfMissing True folder
  return folder

saveFile :: (FilePath, B.ByteString) -> IO FilePath
saveFile (fn, fc) = do
  folder <- fmap (</>fn) $ generateFolder
  _ <- B.writeFile folder fc
  return folder

addHost :: Config -> String -> String
addHost cfg x = (host cfg) ++ ":" ++ (show.port $ cfg) ++ (replace "uploads" "" x)

removeHost :: Config -> String -> String
removeHost cfg x = replace ((host cfg) ++ ":" ++ (show.port $ cfg)) "uploads" x

getConfig :: IO (Either String Config)
getConfig = eitherDecode <$> B.readFile "config.json"

checkKey :: String -> Request -> Bool
checkKey k req = maybe False (== k) reqKey
  where reqKey = fmap BS.unpack $ lookup "Authorization" (requestHeaders req)

keyAuth :: String -> Application -> Request -> IO Response
keyAuth k app req = if ((requestMethod req) == methodOptions) || checkKey k req then app req else return $ responseLBS status401 [] ""

startApp :: Config -> IO ()
startApp cfg = do
   scotty (port cfg) $ do
      middleware logStdoutDev
      middleware $ staticPolicy (addBase "uploads")
--      middleware $ keyAuth (key cfg)
      middleware $ cors getCorsPolicy

      post "/upload" $ do
        xs <- traverse (liftIO . (saveFile . fileToTuple)) =<< files
        let res = T.pack . intercalate [','] . (map (addHost cfg)) $ xs
        json $ object ["success" .= True, "url" .= res ]

      get "/crop" $ do
        command <- param "command"
        url <- param "url"
        res <- liftIO $ cropImage command (removeHost cfg url)
        json $ object ["success" .= True, "url" .= ((addHost cfg) res) ]

      get "/compress" $ do
        command <- param "command"
        url <- param "url"
        res <- liftIO $ compressVideos command (removeHost cfg url)
        json $ object ["success" .= True, "url" .= ((addHost cfg) res) ]

main :: IO()
main = join . fmap (either putStrLn startApp) $ getConfig
