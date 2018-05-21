{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Hue.Endpoint (
    Endpoint (..),
    discover
  )where

import           Control.Exception (throwIO)
import           Data.Aeson        (FromJSON, decode)
import           Data.Text
import           GHC.Generics      (Generic)
import           Network.HTTP.Req

data Endpoint = Endpoint {
    id                :: Text,
    internalipaddress :: Text
  } deriving (Generic, Show)

instance FromJSON Endpoint

instance MonadHttp IO where
  handleHttpException = throwIO

discover :: IO (Maybe [Endpoint])
discover =
  decode . responseBody <$> req GET discoveryUrl NoReqBody lbsResponse mempty

discoveryUrl = https "www.meethue.com" /: "api" /: "nupnp"
