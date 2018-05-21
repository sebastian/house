{-# LANGUAGE OverloadedStrings #-}

module Hue.Light where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text                  as T

type Brightness = Integer
type ColourTemp = Integer

data LightState =
    LightOff
  | LightOn Brightness ColourTemp
  deriving Eq

instance Show LightState where
  show LightOff    = "off"
  show (LightOn b ct) =
    mconcat ["on with brightness: ", show b, ", and colour temperature: ", show ct]

data Light = Light {
    getLightId :: T.Text,
    getName    :: T.Text,
    getState   :: LightState
  } deriving Eq

instance Show Light where
  show (Light i n s) =
    mconcat [T.unpack i, " (", T.unpack n, "): currently ", show s]

parseLight :: T.Text -> Value -> Parser Light
parseLight lightId = withObject "light" $ \o -> do
  stateO <- o .: "state"
  on <- stateO .: "on"
  lightState <- if on
    then LightOn <$> stateO .: "bri" <*> stateO .: "ct"
    else return LightOff
  name <- o .: "name"
  return (Light lightId name lightState)
