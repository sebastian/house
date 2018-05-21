{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Monad               (forever)
import           Control.Monad.State.Lazy
import qualified Data.HashMap.Strict         as SHM
import qualified Data.Text                   as T
import           Data.Time.Clock
import           Data.Time.LocalTime
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import qualified House.Gui                   as Gui
import           House.Room
import qualified Hue
import           Hue.Endpoint                as E
import           Hue.Group
import           Hue.MotionSensor
import           Hue.Reading
import           System.Environment
import           System.Remote.Counter
import           System.Remote.Monitoring

data RuntimeConfig = RuntimeConfig {
  getRoomsMVar        :: MVar Rooms,
  getUser             :: T.Text,
  getEndpoint         :: E.Endpoint,
  getChangeCounter    :: Counter,
  getReadStateCounter :: Counter
}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [u] -> findHueBaseStation $ T.pack u
    _ -> putStrLn $ "ERROR: too many/few arguments. Expected username, got: " ++ show args

findHueBaseStation username = do
  mep <- E.discover
  case mep of
    Nothing -> do
      print "Could not discover a Philips Hue endpoint"
      delayAndRetry username
    Just [] -> do
      print "Could not find any base station to write to"
      delayAndRetry username
    Just (ep:_) -> do
      putStrLn $ "Starting Hue reader with endpoint" ++ show ep
      ekg <- forkServer "localhost" 8000
      roomsMVar <- newMVar SHM.empty
      readStateCounter <- getCounter "app.readHueState" ekg
      changeCounter <- getCounter "app.updateLightState" ekg
      Gui.start roomsMVar
      stateUpdateLoop $ RuntimeConfig roomsMVar username ep changeCounter readStateCounter

delayAndRetry username = do
  print "Will try again in 10 seconds"
  threadDelay 10000000 -- 10 second
  findHueBaseStation username

stateUpdateLoop config = do
  reading <- Hue.getReading (getUser config) (getEndpoint config)
  _ <- inc (getReadStateCounter config)
  processReading reading config
  stateUpdateLoop config

processReading (Right r) config = do
  let roomsAsMap = allRoomsAsMap r
      mvar = getRoomsMVar config
  utcTime <- getCurrentTime
  timeOfDay <- localTimeOfDay . zonedTimeToLocalTime <$> utcToLocalZonedTime utcTime
  roomsState <- takeMVar mvar
  let updatedRoomsState = calculateState utcTime timeOfDay r roomsState
  putMVar mvar updatedRoomsState
  adjustRoomLights config r roomsAsMap $
    calculateLightForRoom timeOfDay utcTime roomsAsMap updatedRoomsState
  threadDelay 500000 -- 0.5 second
processReading (Left e) _config = do
  putStrLn $ "Failed to get readings with error: " ++ e ++ ". Maybe the username is wrong?"
  threadDelay 10000000 -- 10 second

type MapGroupState = SHM.HashMap RoomName GroupState

allRoomsAsMap :: Reading -> MapGroupState
allRoomsAsMap r = foldr setRoomState SHM.empty $ getGroups r
  where setRoomState g m =
          case getGroupType g of
            "Room" -> SHM.insert (getGroupName g) (getGroupState g) m
            _      -> m

adjustRoomLights :: RuntimeConfig -> Reading -> MapGroupState -> MapGroupState -> IO ()
adjustRoomLights config r currentState desiredState =
  mapM_ (adjustLight config r) $ SHM.toList $ changedLights currentState desiredState

adjustLight config reading (room, desiredState) =
  case filter (\g -> getGroupName g == room) (getGroups reading) of
    [g] -> do
      _ <- inc $ getChangeCounter config
      Hue.setGroupState (getUser config) (getEndpoint config) (getGroupId g) desiredState
    _   -> print $ "Could not adjust the lights for room " ++ T.unpack room ++ ". Can't determine the ID"

changedLights currentState =
  SHM.filterWithKey (onlyChangedLights currentState)

onlyChangedLights :: MapGroupState -> RoomName -> GroupState -> Bool
onlyChangedLights currentState room desiredState =
  SHM.lookup room currentState /= Just desiredState

calculateLightForRoom :: TimeOfDay -> UTCTime -> MapGroupState -> Rooms -> MapGroupState
calculateLightForRoom localTime utcTime mgs =
  SHM.foldrWithKey (adaptRoom localTime utcTime) (createInitialRoomState mgs)

adaptRoom localTimeOfDay _utcTime name (Primary _) m = SHM.insert name state m
  where state
          | isAtNight localTimeOfDay && name == "Bedroom" = GroupOff
          | otherwise = groupStateForBi (briByTimeOfDay localTimeOfDay) localTimeOfDay
adaptRoom localTimeOfDay utcTime name (Secondary startTime) m = SHM.insert name state m
  where state
          | isAtNight localTimeOfDay = GroupOff
          | otherwise =
              let maxSec = 10 * secondsPerMinute
                  elapsedTime = min maxSec (round $ diffUTCTime utcTime startTime)
                  defaultBri = briByTimeOfDay localTimeOfDay
                  minBri = round $ toRational defaultBri / 4.0
                  scaledBri = scaleDown 0 maxSec defaultBri minBri elapsedTime in
              groupStateForBi scaledBri localTimeOfDay

groupStateForBi :: Integer -> TimeOfDay -> GroupState
groupStateForBi 0 _ = GroupOff
groupStateForBi bri localTimeOfDay = GroupOn bri (ctByTimeOfDay localTimeOfDay)

-- 153 to 450 where higher is warmer
ctByTimeOfDay currentTime
              | timeOfDayAsSec currentTime < timeInSec 6 00 = 450
              | timeOfDayAsSec currentTime < timeInSec 12 00 = 153
              | timeOfDayAsSec currentTime < timeInSec 15 00 =
                  scaleUp (hourInSec 12) (hourInSec 15) 153 350 (timeOfDayAsSec currentTime)
              | timeOfDayAsSec currentTime < timeInSec 19 00 =
                  scaleUp (hourInSec 15) (hourInSec 19) 350 450 (timeOfDayAsSec currentTime)
              | otherwise = 450

briByTimeOfDay currentTime
              | timeOfDayAsSec currentTime < timeInSec 6 00 = 0
              | timeOfDayAsSec currentTime < timeInSec 17 00 = 254
              | timeOfDayAsSec currentTime < timeInSec 22 00 =
                  scaleDown (hourInSec 17) (hourInSec 21) 254 100 (timeOfDayAsSec currentTime)
              | otherwise = 50

isAtNight :: TimeOfDay -> Bool
isAtNight tod = (tis > timeInSec 10 00) && (tis < timeInSec 6 00)
  where tis = timeOfDayAsSec tod

scaleUp :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
scaleUp inputStart inputEnd outputStart outputEnd current =
  let currentFraction = toRational (current - inputStart) / toRational (inputEnd - inputStart)
      outputFraction = toRational outputStart + toRational (outputEnd - outputStart) * currentFraction in
  round outputFraction

scaleDown :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
scaleDown inputStart inputEnd outputStart outputEnd current =
  let currentFraction = toRational (current - inputStart) / toRational (inputEnd - inputStart)
      outputFraction = toRational outputStart - toRational (outputStart - outputEnd) * currentFraction in
  round outputFraction

timeOfDayAsSec :: TimeOfDay -> Integer
timeOfDayAsSec tod =
  toInteger (todHour tod) * secondsPerHour + toInteger (todMin tod) * secondsPerMinute + round (todSec tod)

timeInSec :: Integer -> Integer -> Integer
timeInSec hour minute = hour * secondsPerHour + minute * secondsPerMinute

hourInSec :: Integer -> Integer
hourInSec hour = timeInSec hour 0

secondsPerMinute = 60
secondsPerHour = secondsPerMinute * 60

createInitialRoomState = SHM.map (const GroupOff)


calculateState :: UTCTime -> TimeOfDay -> Reading -> Rooms -> Rooms
calculateState now localTime reading rooms =
  let sensors = getMotionSensors reading
      activeRooms = filter (\s -> getSensorState s /= NoPresence) sensors in
  removeOrphanSecondaryRooms $ removeOldMainRoom now localTime $ foldr addRoom rooms activeRooms

addRoom sensor s0 =
  let n = getSensorName sensor
      Presence time = getSensorState sensor in
  case SHM.lookup n s0 of
    Nothing ->
      addPrimary time n s0
    Just (Secondary _) ->
      addPrimary time n s0
    Just (Primary originalTime) ->
      if diffUTCTime time originalTime > 0
        then SHM.insert n (Primary time) s0
        else s0

addPrimary time room s0 =
  let s1 = SHM.insert room (Primary time) s0 in
  foldr (addSecondary time) s1 $ neighbouringRooms room

addSecondary :: UTCTime -> RoomName -> Rooms -> Rooms
addSecondary time room s0 =
  case SHM.lookup room s0 of
    Nothing -> SHM.insert room (Secondary time) s0
    Just _  -> s0

removeOldMainRoom :: UTCTime -> TimeOfDay -> Rooms -> Rooms
removeOldMainRoom currentTime localTime =
  SHM.filter (filterExpiredMain currentTime localTime)

maxRoomTime :: TimeOfDay -> Integer
maxRoomTime localTime
             | isAtNight localTime = secondsPerMinute
             | otherwise = 20 * secondsPerMinute

filterExpiredMain :: UTCTime -> TimeOfDay -> RoomState -> Bool
filterExpiredMain _ _ (Secondary _) = True
filterExpiredMain now localTime (Primary roomTime) =
  round (diffUTCTime now roomTime) < maxRoomTime localTime

removeOrphanSecondaryRooms :: Rooms -> Rooms
removeOrphanSecondaryRooms rooms =
  SHM.filterWithKey (filterOrphanRoom rooms) rooms

filterOrphanRoom :: Rooms -> RoomName -> RoomState -> Bool
filterOrphanRoom _ _ (Primary _) = True -- Primary rooms cannot be orphans
filterOrphanRoom rooms roomName (Secondary _) =
  any (`SHM.member` rooms) $ neighbouringRooms roomName

neighbouringRooms "Kitchen"     = ["Hallway"]
neighbouringRooms "Hallway"     = ["Kitchen", "Bathroom", "Living room"]
neighbouringRooms "Bathroom"    = ["Hallway"]
neighbouringRooms "Living room" = ["Hallway", "Bedroom"]
neighbouringRooms "Bedroom"     = ["Living room"]
neighbouringRooms _             = []
