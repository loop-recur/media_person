module MediaConversion where

import Data.Map.Strict (Map, fromList, (!))

import System.FilePath (replaceExtension, takeExtension)
import System.Process (callProcess)

type VideoFormat = String

data ConversionOpts = ConversionOpts {
  convOpts      :: [String]
, convExtension :: String
} deriving (Eq, Show)

conversions :: Map VideoFormat ConversionOpts
conversions = fromList [
    ("h264", ConversionOpts ["-vcodec","libx264","-preset","fast","-crf","22"]              ".mp4" )
  , ("ogg",  ConversionOpts ["-c:v","libtheora","-c:a","libvorbis","-q:v","10","-q:a","10"] ".ogv" )
  ]

isVideo :: FilePath -> Bool
isVideo = flip elem videoExtensions . takeExtension
  where
    videoExtensions = [".avi", ".wmv", ".flv", ".mpg", ".mpeg", ".mp4", ".mov", ".m4v"]

convertedName :: ConversionOpts -> FilePath -> FilePath
convertedName = flip replaceExtension . convExtension

compressVideo :: VideoFormat -> FilePath -> FilePath -> IO ()
compressVideo fmt input output = callProcess "ffmpeg" $ ["-i", input] ++ convOpts conv ++ [output]
  where conv = conversions ! fmt :: ConversionOpts

videoScreenshot :: FilePath -> FilePath -> IO ()
videoScreenshot input output =
  callProcess "ffmpeg" ["-i", input, "-vframes","1","-f","image2","-an", output]
--  let dest = replaceExtension ".jpg" input

convertImage :: [String] -> FilePath -> FilePath -> IO ()
convertImage params input output = callProcess "convert" $ [input] ++ params ++ [output]
