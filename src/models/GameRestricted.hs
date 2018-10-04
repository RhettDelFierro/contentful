{-# LANGUAGE OverloadedStrings #-}

module Models.GameRestricted where

import Models.GlobalModels (SysItem, SysLink)
import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format

data AllGameRestrictedQuery = AllGameRestrictedQuery {
  topSys :: SysLink
  , total    :: Integer
  , skip     :: Integer
  , limit    :: Integer
  , items    :: [GameRestrictedItem]
} deriving (Show, Eq)

instance FromJSON AllGameRestrictedQuery where
    parseJSON (Object o) =
      AllGameRestrictedQuery <$> (o .: "sys")
                             <*> (o .: "total")
                             <*> (o .: "skip")
                             <*> (o .: "limit")
                             <*> (o .: "items")
    parseJSON _          = mzero

data GameRestrictedItem = GameRestrictedItem {
  sys :: SysItem
  , fields :: GameRestrictedField
} deriving (Show, Eq)

instance FromJSON GameRestrictedItem where
    parseJSON (Object o) = GameRestrictedItem <$> (o .: "sys") <*> (o .: "fields")
    parseJSON _          = mzero

data GameRestrictedField = GameRestrictedField {
  visibleIfUnowned :: Bool
  , uuid :: String
  , favoriteOnDiscovery :: Bool
} deriving (Show,Eq)

instance FromJSON GameRestrictedField where
    parseJSON (Object o) = 
        GameRestrictedField <$> (o .: "visibleIfUnowned") 
                            <*> (o .: "uuid")
                            <*> (o .: "favoriteOnDiscovery")
    parseJSON _          = mzero