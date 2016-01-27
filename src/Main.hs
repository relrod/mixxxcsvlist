{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Csv
import Data.List.Split
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Units
import qualified Data.Vector as V
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

data Song = Song {
    songNumber :: String
  , songTimestamp :: String
  , songPreview :: String
  , songPlayed :: String
  , songArtist :: String
  , songTitle :: String
  , songAlbum :: String
  , songYear :: String
  , songDuration :: String
  , songRating :: String
  , songGenre :: String
  , songComposer :: String
  , songType :: String
  , songTrackNumber :: String
  , songKey :: String
  , songDateAdded :: String
  , songBPM :: String
  , songBitrate :: String
  , songLocation :: String
  , songComment :: String
  } deriving (Eq, Show)

instance FromNamedRecord Song where
  parseNamedRecord s =
    Song
    <$> s .: "#"
    <*> s .: "Timestamp"
    <*> s .: "Preview"
    <*> s .: "Played"
    <*> s .: "Artist"
    <*> s .: "Title"
    <*> s .: "Album"
    <*> s .: "Year"
    <*> s .: "Duration"
    <*> s .: "Rating"
    <*> s .: "Genre"
    <*> s .: "Composer"
    <*> s .: "Type"
    <*> s .: "Track #"
    <*> s .: "Key"
    <*> s .: "Date Added"
    <*> s .: "BPM"
    <*> s .: "Bitrate"
    <*> s .: "Location"
    <*> s .: "Comment"

-- | Attempt to parse time duration into seconds.
parseDuration :: String -> Maybe Integer
parseDuration s =
  case splitOn ":" s of
    (min:sec:_) -> do
      minInteger <- readMaybe min :: Maybe Integer
      secInteger <- readMaybe sec :: Maybe Integer
      return (fromIntegral (addTime (tm minInteger) (ts secInteger) :: Second))
    _ -> Nothing
  where
    ts :: Integer -> Second
    ts = fromIntegral

    tm :: Integer -> Minute
    tm = fromIntegral

-- | Convert seconds into human readable form.
humanReadableDuration :: Integer -> String
humanReadableDuration i =
  let hours = i `div` 3600
      minutes = (i - (hours * 3600)) `div` 60
      seconds = (i - (hours * 3600) - (minutes * 60))
  in show hours ++ "h " ++ show minutes ++ "m " ++ show seconds ++ "s"

-- | Given a vector of songs, parse and add up all the durations, showing the
-- result in human readable form.
totalDurations :: V.Vector Song -> Maybe String
totalDurations v =
  let maybeParses = V.map (parseDuration . songDuration) v
  in if all isJust maybeParses
     then return . humanReadableDuration . V.sum . catVectorMaybes $ maybeParses
     else Nothing
  where
    catVectorMaybes v = V.map fromJust . V.filter isJust $ v

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ die "Usage: mixxxcsvlist /path/to/playlist.csv"
  let filename = head args
  x <- C8.readFile filename
  case decodeByName x :: Either String (Header, V.Vector Song) of
    Left e -> error (show e)
    Right (h, v) -> do
      V.mapM_ (putStrLn . prettySongLine) v
      putStrLn "----------------------------------------------------"
      putStrLn $ "Total Duration: " ++ fromMaybe "Unknown" (totalDurations v)

prettySongLine :: Song -> String
prettySongLine s =
  songNumber s ++ ". " ++ songArtist s ++ " - " ++ songTitle s
  ++ " - " ++ songDuration s
