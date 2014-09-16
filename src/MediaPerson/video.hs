{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module MediaPerson.Video(
  saveFile,
  compressVideos,
  cropImage
) where

import Control.Monad.IO.Class
import Control.Applicative((<$>))
import Control.Concurrent(forkIO)
import Data.Char(toLower)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS

import System.FilePath((</>), takeExtension)
import System.Random(newStdGen, randomRs)
import System.Directory(createDirectoryIfMissing)
import System.Process(readProcessWithExitCode, readProcess)
import Data.String.Utils(replace)
import Data.List.Split(splitOn)
import Data.Map.Strict(Map, (!), fromList)
import GHC.Generics

preset_map :: Map String (String, String)
preset_map = fromList [("h264", ("-vcodec,libx264,-preset,fast,-crf,22", ".mp4")), ("ogg", ("-c:v,libtheora,-c:a,libvorbis,-q:v,10,-q:a,10", ".ogv"))]

getPresent :: String -> (String, String)
getPresent x = preset_map ! x

video_extensions :: [String]
video_extensions = [".avi", ".wmv", ".flv", ".mpg", ".mpeg", ".mp4", ".mov", ".m4v"]

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
compressVideos command input_file = ((mapM_ (compressVideo input_file)) . splitOn "," $ command) >> (return input_file)

doVideoThings :: FilePath -> IO FilePath
doVideoThings input_file = do
  output_file <- getScreenshot input_file
  _ <- compressVideos "h264,ogg" input_file
  return output_file

generateFolder :: IO FilePath
generateFolder = do
  folder <- fmap (("uploads"</>) . take 10 . randomRs ('a','z')) newStdGen
  createDirectoryIfMissing True folder
  return folder

isVideo :: FilePath -> Bool
isVideo = ((flip elem) video_extensions) . takeExtension . (map toLower)

saveFile :: (FilePath, B.ByteString) -> IO FilePath
saveFile (fn, fc) = do
  folder <- fmap (</>fn) $ generateFolder
  _ <- B.writeFile folder fc
  if (isVideo fn) then (doVideoThings folder) else (return folder)

