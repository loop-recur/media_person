{-# LANGUAGE OverloadedStrings, RankNTypes, DeriveGeneric #-}
import Web.Scotty

import Control.Monad.IO.Class
import Control.Monad(join)
import Control.Applicative((<$>))
import Data.Traversable(traverse)
import Data.List(intercalate)

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors
import Network.Wai.Parse

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as T
import Data.Aeson(FromJSON,eitherDecode,decode,object,(.=)) --JSON
import qualified Data.HashMap.Strict as HM

import System.FilePath((</>))
import System.Random(newStdGen, randomRs)
import System.Directory(createDirectoryIfMissing)
import System.Process(readProcessWithExitCode)
import Data.String.Utils(replace)
import Data.List.Split(splitOn)
import GHC.Generics

data Config = Config { host :: String, port :: Int } deriving (Show, Generic)
instance FromJSON Config

getCorsPolicy :: a -> Maybe CorsResourcePolicy
getCorsPolicy = const $ Just (CorsResourcePolicy { corsMethods=["GET", "PUT", "POST"], corsOrigins=Nothing, corsRequestHeaders=["x-requested-with", "content-type", "cache-control"], corsExposedHeaders=(Just ["Access-Control-Allow-Origin"]), corsMaxAge=(Just 1000), corsVaryOrigin=True, corsVerboseResponse=True  })

fileToTuple :: forall t t1. (t, FileInfo t1) -> ([Char], t1)
fileToTuple (_, fi) = (BS.unpack (fileName fi), fileContent fi)

insertFile :: FilePath -> String -> FilePath
insertFile path x = fn ++ x ++ ext
  where (fn, ext) = break (=='.') $ path

getPathName :: FilePath -> IO FilePath
getPathName path = fmap ((insertFile path) . take 4 . randomRs ('a','z')) $ newStdGen

makeImArgs :: FilePath -> FilePath -> String -> [String]
makeImArgs path path_name command = ([path]++cmds++[path_name])
  where cmds = splitOn "," command

cropImage :: String -> FilePath -> IO FilePath
cropImage command path = do
  path_name <- getPathName path
  _ <- readProcessWithExitCode "convert" (makeImArgs path path_name command) ""
  return path_name

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

getConfig :: IO B.ByteString
getConfig = B.readFile "config.json"

main = do
 d <- (eitherDecode <$> getConfig) :: IO (Either String Config)
 case d of
   Left err -> putStrLn err
   Right cfg -> scotty (port cfg) $ do
      middleware logStdoutDev
      middleware $ staticPolicy (addBase "uploads")
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

