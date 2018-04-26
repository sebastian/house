module Hue.MotionSensor where

import qualified Data.Text       as T
import           Data.Time.Clock (UTCTime)

data MotionSensorState =
    NoPresence
  | Presence (Maybe UTCTime)
  deriving Eq

data MotionSensor = MotionSensor {
    getSensorId    :: T.Text,
    getSensorName  :: T.Text,
    getSensorState :: MotionSensorState
  } deriving Eq

instance Show MotionSensor where
  show (MotionSensor sid n NoPresence) = mconcat [T.unpack sid, " (", T.unpack n, ") – not present"]
  show (MotionSensor sid n (Presence time)) = mconcat [T.unpack sid, " (", T.unpack n, ") – last active ", show time]
