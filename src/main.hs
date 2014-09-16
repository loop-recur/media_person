{-# LANGUAGE OverloadedStrings, RankNTypes, DeriveGeneric #-}
import Web.Scotty
import MediaPerson.Video
import Control.Monad.IO.Class
import Control.Monad(join)
import Control.Applicative((<$>))
import Data.Traversable(traverse)
import Data.List(intercalate)

import Network.Wai (Application, Request(..), Response, responseLBS)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse
import Network.HTTP.Types (status401, methodOptions)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as T
import Data.String.Utils(replace)
import Data.Aeson(FromJSON,eitherDecode,object,(.=)) --JSON
import GHC.Generics

data Config = Config { host :: String, port :: Int, key :: String } deriving (Show, Generic)
instance FromJSON Config

addHost :: Config -> String -> String
addHost cfg x = (host cfg) ++ (replace "uploads" "" x)

removeHost :: Config -> String -> String
removeHost cfg x = replace (host cfg) "uploads" x

getConfig :: IO (Either String Config)
getConfig = eitherDecode <$> B.readFile "config.json"

checkKey :: String -> Request -> Bool
checkKey k req = maybe False (== k) reqKey
  where reqKey = fmap BS.unpack $ lookup "Authorization" (requestHeaders req)

keyAuth :: String -> Application -> Request -> IO Response
keyAuth k app req = if ((requestMethod req) == methodOptions) || checkKey k req then app req else return $ responseLBS status401 [] ""

fileToTuple :: forall t t1. (t, FileInfo t1) -> ([Char], t1)
fileToTuple (_, fi) = (BS.unpack (fileName fi), fileContent fi)

startApp :: Config -> IO ()
startApp cfg = do
   scotty (port cfg) $ do
      middleware logStdoutDev
      middleware $ staticPolicy (addBase "uploads")
--      middleware $ keyAuth (key cfg)

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
