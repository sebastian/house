module Hue.Light where

import qualified Data.Text as T

type Brightness = Int
type Hue = Maybe Int
type Saturation = Maybe Int

data LightState = LightOff
                | LightOn Brightness Hue Saturation
                deriving Eq

instance Show LightState where
  show LightOff = "off"
  show (LightOn b h s) = mconcat ["on with brightness: ", show b, ", hue: ", show h, ", and saturation: ", show s]

data Light = Light {
    lightId :: T.Text,
    state   :: LightState,
    name    :: T.Text
  } deriving Eq

instance Show Light where
  show (Light i s n) = mconcat [T.unpack i, " (", T.unpack n, "): currently ", show s]
