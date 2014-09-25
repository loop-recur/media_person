module MediaConversion where

import Data.Map.Strict (Map, fromList)

import System.FilePath (replaceExtension, takeExtension)
import System.Process (callProcess, readProcessWithExitCode)
import System.Exit

type VideoFormat = String

data ConversionOpts = ConversionOpts {
  convOpts      :: [String]
, convExtension :: String
} deriving (Eq, Show)

conversions :: Map VideoFormat ConversionOpts
conversions = fromList [
    ("h264", ConversionOpts ["-y","-vcodec","libx264","-preset","fast","-crf","22"]              ".mp4" )
  , ("ogg",  ConversionOpts ["-y","-c:v","libtheora","-c:a","libvorbis","-q:v","10","-q:a","10"] ".ogv" )
  , ("jpeg", ConversionOpts ["-y","-vframes","1","-f","image2","-an"] ".jpg" )
  ]

isVideo :: FilePath -> Bool
isVideo = flip elem videoExtensions . takeExtension
  where
    videoExtensions = [".avi", ".wmv", ".flv", ".mpg", ".mpeg", ".mp4", ".mov", ".m4v"]

convertedName :: ConversionOpts -> FilePath -> FilePath
convertedName = flip replaceExtension . convExtension

compressVideo :: ConversionOpts -> FilePath -> FilePath -> IO ()
compressVideo conv input output =
  callProcess "ffmpeg" $ ["-i", input] ++ convOpts conv ++ [output]

convertImage :: [String] -> FilePath -> FilePath -> IO(Either String FilePath)
convertImage params input output = do
  (code, _, e) <- readProcessWithExitCode "convert" ([input] ++ params ++ [output]) ""
  case code of
    ExitSuccess -> return $ Right output
    ExitFailure _ -> return $ Left e
