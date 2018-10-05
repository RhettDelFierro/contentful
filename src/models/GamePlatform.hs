{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.GamePlatform where

import Models.GlobalModels (SysItem, SysLink)
import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format

data GamePlatformItem = GamePlatformItem {
  sys :: SysItem
  , fields :: GamePlatformField
} deriving (Show, Eq)

instance FromJSON GamePlatformItem where
    parseJSON (Object o) = GamePlatformItem <$> (o .: "sys") <*> (o .: "fields")
    parseJSON _          = mzero

data GamePlatformField = GamePlatformField {
  entryTitle :: String
  , platform :: SysLink
} deriving (Show,Eq)

instance FromJSON GamePlatformField where
    parseJSON (Object o) = 
        GamePlatformField <$> (o .: "entryTitle") 
                      <*> (o .: "platform")
    parseJSON _          = mzero