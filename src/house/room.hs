module House.Room where

import qualified Data.HashMap.Strict as SHM
import qualified Data.Text           as T
import           Data.Time.Clock

data RoomState =
    Primary UTCTime
  | Secondary UTCTime
  deriving (Show)

type RoomName = T.Text
type Rooms = SHM.HashMap RoomName RoomState
