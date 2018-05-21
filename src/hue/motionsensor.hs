{-# LANGUAGE OverloadedStrings #-}

module Hue.MotionSensor (
    MotionSensorState (..),
    MotionSensor (..),
    parseSensor
  ) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text                  as T
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Format           (defaultTimeLocale, parseTimeM)

data MotionSensorState =
    NoPresence
  | Presence UTCTime
  deriving Eq

data MotionSensor = MotionSensor {
    getSensorId    :: T.Text,
    getSensorName  :: T.Text,
    getSensorState :: MotionSensorState
  } deriving Eq

instance Show MotionSensor where
  show (MotionSensor sid n NoPresence) = mconcat [T.unpack sid, " (", T.unpack n, ") – not present"]
  show (MotionSensor sid n (Presence time)) = mconcat [T.unpack sid, " (", T.unpack n, ") – last active ", show time]

parseLastUpdated :: Text -> MotionSensorState
parseLastUpdated t =
  case parseTimeM True defaultTimeLocale "%FT%T%Q" $ T.unpack t of
    Just time -> Presence time
    Nothing   -> NoPresence

parseSensor :: Text -> Value -> Parser (Maybe MotionSensor)
parseSensor sensorId = withObject "motion sensor" $ \o -> do
  sensorType <- (o .:? "type" .!= "wrong-type") :: Parser Text
  if sensorType /= "ZLLPresence" then
    return Nothing
  else do
    stateO <- o .: "state"
    presence <- stateO .: "presence"
    presenceState <- if presence
      then parseLastUpdated <$> stateO .: "lastupdated"
      -- then Presence <$> (parseLastUpdated <$> stateO .: "lastupdated")
      else return NoPresence
    name <- o .: "name"
    return $ Just (MotionSensor sensorId name presenceState)
