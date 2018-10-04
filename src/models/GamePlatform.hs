{-# LANGUAGE OverloadedStrings #-}

module Models.GamePlatforms where

import Models.GlobalModels (SysItem, SysLink)
import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format

data AllGamePlatformsQuery = AllGamePlatformsQuery {
  topGamesSys :: SysLink
  , total    :: Integer
  , skip     :: Integer
  , limit    :: Integer
  , items    :: [GamePlatformItem]
} deriving (Show, Eq)

instance FromJSON AllGamePlatformsQuery where
    parseJSON (Object o) =
      AllGamePlatformsQuery <$> (o .: "sys")
                            <*> (o .: "total")
                            <*> (o .: "skip")
                            <*> (o .: "limit")
                            <*> (o .: "items")
    parseJSON _          = mzero

data GamePlatformItem = GamePlatformItem {
  platformSys :: SysItem
  , platformItemFields :: GamePlatformField
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