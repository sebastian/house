{-# LANGUAGE OverloadedStrings #-}

module Hue.Group where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text                  as T

type Brightness = Integer
type ColourTemp = Integer

type GroupId = T.Text

data GroupState =
    GroupOff
  | GroupOn Brightness ColourTemp
  deriving Eq

instance Show GroupState where
  show GroupOff    = "off"
  show (GroupOn b ct) =
    mconcat ["on with brightness: ", show b, ", and colour temperature: ", show ct]

data Group = Group {
    getGroupId       :: GroupId,
    getGroupName     :: T.Text,
    getGroupLightIds :: [T.Text],
    getGroupType     :: T.Text,
    getGroupState    :: GroupState
  } deriving Eq

instance Show Group where
  show (Group i n ls t s) =
    mconcat [T.unpack i, " (", T.unpack n, " of type ", T.unpack t, ") has lights: ", show ls, " – ", show s]

parseGroup :: T.Text -> Value -> Parser Group
parseGroup groupId = withObject "group" $ \o -> do
  actionO <- o .: "action"
  on <- actionO .: "on"
  state <- if on
    then GroupOn <$> actionO .: "bri" <*> actionO .: "ct"
    else return GroupOff
  name <- o .: "name"
  groupType <- o .: "type"
  lights <- o .: "lights"
  return (Group groupId name lights groupType state)
