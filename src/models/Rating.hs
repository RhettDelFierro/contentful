{-# LANGUAGE OverloadedStrings #-}

module Models.Rating where

import Models.GlobalModels (SysItem, SysLink)
import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format


data RatingItem = RatingItem {
  sys :: SysItem
  , fields :: RatingField
} deriving (Show, Eq)

instance FromJSON RatingItem where
    parseJSON (Object o) = RatingItem <$> (o .: "sys") <*> (o .: "fields")
    parseJSON _          = mzero

data RatingField = RatingField {
  name :: String
  , ratingSystem :: SysLink
  , shortName :: String
  , ratingImage :: SysLink
  , key :: String
} deriving (Show,Eq)

instance FromJSON RatingField where
    parseJSON (Object o) = 
        RatingField <$> (o .: "name") 
                    <*> (o .: "ratingSystem")
                    <*> (o .: "shortName")
                    <*> (o .: "ratingImage")
                    <*> (o .: "key")
    parseJSON _          = mzero