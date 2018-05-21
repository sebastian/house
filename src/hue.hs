{-# LANGUAGE OverloadedStrings #-}

module Hue (getReading, setGroupState) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson                 (FromJSON, decode, encode)
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict        as SHM
import           Data.Scientific            (scientific)
import           Data.Text
import           GHC.Generics               (Generic)
import           Hue.Endpoint
import           Hue.Group
import qualified Hue.Reading                as R
import           Network.HTTP.Req

type Username = Text

data State = State {
    endpoint :: Endpoint,
    username :: Text
  } deriving (Show)


---------------------------------------------------------------------
-- Writing to Hue base station
---------------------------------------------------------------------

setGroupState :: Username -> Endpoint -> GroupId -> GroupState -> IO ()
setGroupState un ep gid gs = do
  let url = setGroupStateUrl (State ep un) gid
      message = messageFor gs
  void (req PUT url (ReqBodyJson message) ignoreResponse mempty) `catch` handleWriteFailure

messageFor GroupOff = Object $ SHM.fromList [("on", Bool False)]
messageFor (GroupOn b ct) =
  Object $ SHM.fromList [("on", Bool True), ("bri", asScientificNumber b), ("ct", asScientificNumber ct)]
  where asScientificNumber n = Number $ scientific n 0

setGroupStateUrl state id = http ip /: "api" /: un /: "groups" /: id /: "action"
  where ip = internalipaddress $ endpoint state
        un = username state

handleWriteFailure :: HttpException -> IO ()
handleWriteFailure e = do
  print $ "Failed at writing changes to Hue base station: " ++ show e
  return ()


---------------------------------------------------------------------
-- Reading from Hue base station
---------------------------------------------------------------------

getReading :: Username -> Endpoint -> IO (Either String R.Reading)
getReading un ep = do
  let url = readingUrl $ State ep un
  (R.parse . responseBody <$> req GET url NoReqBody lbsResponse mempty) `catch` readException

readingUrl state = http ip /: "api" /: un
  where ip = internalipaddress $ endpoint state
        un = username state

readException :: HttpException -> IO (Either String R.Reading)
readException ex =
  return $ Left $ "Reading from Hue failed with exception: " ++ show ex
