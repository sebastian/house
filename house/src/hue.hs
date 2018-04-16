{-# LANGUAGE OverloadedStrings #-}

module Hue (getReading) where

import           Control.Exception          (throwIO)
import           Data.Aeson                 (FromJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text
import           GHC.Generics               (Generic)
import qualified Hue.Endpoint               as E
import qualified Hue.Reading                as R
import           Network.HTTP.Req

type Username = Text

data State = State {
    endpoint :: E.Endpoint,
    username :: Text
  } deriving (Show)

getReading :: Username -> IO (Either String R.Reading)
getReading un = do
  mep <- E.discover
  case mep of
    Just (ep:_) -> do
      let url = readingUrl $ State ep un
      R.parse . responseBody <$> req GET url NoReqBody lbsResponse mempty
    Nothing ->
      return $ Left "Could not discover a Philips Hue endpoint"

readingUrl state = http ip /: "api" /: un
  where ip = E.internalipaddress $ endpoint state
        un = username state
