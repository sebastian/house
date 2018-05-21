{-# LANGUAGE OverloadedStrings #-}

module House.Gui (start) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.HashMap.Strict         as SHM
import qualified Data.Text                   as T
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import           House.Room

start :: MVar Rooms -> IO ThreadId
start roomsMVar = forkIO (startGUI defaultConfig $ setupGui roomsMVar)

setupGui :: MVar Rooms -> Window -> UI ()
setupGui mvar window = void $ do
  roomsState <- liftIO $ readMVar mvar
  return window # set UI.title "House - Lights"
  ul <- UI.ul #+ rooms roomsState
  getBody window #+ [element ul]

rooms :: Rooms -> [UI Element]
rooms state = map produceRoom $ SHM.toList state

produceRoom :: (T.Text, RoomState) -> UI Element
produceRoom (name, Primary _)   = UI.li # set html ("Primary room: " ++ T.unpack name)
produceRoom (name, Secondary _) = UI.li # set html ("Secondary room: " ++ T.unpack name)
