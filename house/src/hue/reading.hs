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
import           Hue.Group
import           Hue.Light
import           Hue.MotionSensor

data Reading = Reading {
    getLights        :: [Light],
    getMotionSensors :: [MotionSensor],
    getGroups        :: [Group]
  } deriving (Eq, Show)

-- data Reading = Reading {
--     getLights        :: [Light],
--     getMotionSensors :: [MotionSensor],
--     getGroups        :: [Group]
--   } deriving (Eq, Show)


parseLights :: Value -> Parser [Light]
parseLights = withObject "lights" $ \o -> do
  let mapper (lid, lightO) = parseLight lid lightO
  traverse mapper (HM.toList o)

parseSensors :: Value -> Parser [Maybe MotionSensor]
parseSensors = withObject "sensors" $ \o -> do
  let mapper (sId, sensorO) = parseSensor sId sensorO
  traverse mapper $ HM.toList o

parseGroups :: Value -> Parser [Group]
parseGroups = withObject "groups" $ \o -> do
  let mapper (gid, groupO) = parseGroup gid groupO
  traverse mapper (HM.toList o)

reading :: Value -> Parser Reading
reading = withObject "HUE payload" $ \o -> do
  lightsO <- o .: "lights"
  lights <- parseLights lightsO
  sensorsO <- o .: "sensors"
  sensors <- parseSensors sensorsO
  groupsO <- o .: "groups"
  groups <- parseGroups groupsO
  return (Reading lights (catMaybes sensors) groups)

parse :: LBS.ByteString -> Either String Reading
parse json =
  case decode json of
    Just c  -> parseEither reading c
    Nothing -> Left "Couldn't decode"
