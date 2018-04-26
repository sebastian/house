{-# LANGUAGE OverloadedStrings #-}

module Hue.Reading (parse, Reading (..)) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types           hiding (parse)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Functor
import           Data.HashMap.Strict        as HM
import           Data.Maybe
import           Data.Text                  as T
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Format           (defaultTimeLocale, parseTimeM)
import           Hue.Light
import           Hue.MotionSensor

data Reading = Reading {
    getLights        :: [Light],
    getMotionSensors :: [MotionSensor]
  } deriving (Eq, Show)

parseLight :: Text -> Value -> Parser Light
parseLight lightId = withObject "light" $ \o -> do
  stateO <- o .: "state"
  on <- stateO .: "on"
  lightState <- if on
    then LightOn <$> stateO .: "bri" <*> stateO .: "ct"
    else return LightOff
  name <- o .: "name"
  return (Light lightId name lightState)

parseLights :: Value -> Parser [Light]
parseLights = withObject "lights" $ \o -> do
  let mapper (lid, lightO) = parseLight lid lightO
  traverse mapper (HM.toList o)

parseLastUpdated :: Text -> Maybe UTCTime
parseLastUpdated t = parseTimeM True defaultTimeLocale "%FT%T%Q" $ T.unpack t

parseSensor :: Text -> Value -> Parser (Maybe MotionSensor)
parseSensor sensorId = withObject "motion sensor" $ \o -> do
  sensorType <- (o .:? "type" .!= "wrong-type") :: Parser Text
  if sensorType /= "ZLLPresence" then
    return Nothing
  else do
    stateO <- o .: "state"
    presence <- stateO .: "presence"
    presenceState <- if presence
      then Presence <$> (parseLastUpdated <$> stateO .: "lastupdated")
      else return NoPresence
    name <- o .: "name"
    return $ Just (MotionSensor sensorId name presenceState)

parseSensors :: Value -> Parser [Maybe MotionSensor]
parseSensors = withObject "sensors" $ \o -> do
  let mapper (sId, sensorO) = parseSensor sId sensorO
  traverse mapper $ HM.toList o

reading :: Value -> Parser Reading
reading = withObject "HUE payload" $ \o -> do
  lightsO <- o .: "lights"
  lights <- parseLights lightsO
  sensorsO <- o .: "sensors"
  sensors <- parseSensors sensorsO
  return (Reading lights (catMaybes sensors))

parse :: LBS.ByteString -> Either String Reading
parse json =
  case decode json of
    Just c  -> parseEither reading c
    Nothing -> Left "Couldn't decode"
