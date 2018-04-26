module Hue.Light where

import qualified Data.Text as T

type Brightness = Int
type ColourTemp = Int

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
