module ServerUtils where

import qualified Data.ByteString.Lazy as BL

import GHC.Generics
import Control.Applicative((<$>))
import Data.Aeson(FromJSON, eitherDecode)
import Data.String.Utils(replace)

import Network.Wai (Request(..), requestHeaders)
import Network.Wai.Middleware.Cors

data Config = Config { host :: String, port :: Int } deriving (Show, Generic)
instance FromJSON Config

getConfig :: IO (Either String Config)
getConfig = eitherDecode <$> BL.readFile "config.json"

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

addHost :: Config -> String -> String
addHost cfg x = host cfg ++ ":" ++ (show.port $ cfg) ++ replace "uploads" "" x

removeHost :: Config -> String -> String
removeHost cfg = replace (host cfg ++ ":" ++ (show.port $ cfg)) "uploads"
