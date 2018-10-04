{-# LANGUAGE OverloadedStrings #-}

module Models.RatingDescription where

import Models.GlobalModels (SysItem, SysLink)
import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format

data AllRatingDescriptionQuery = AllRatingDescriptionQuery {
  topSys :: SysLink
  , total    :: Integer
  , skip     :: Integer
  , limit    :: Integer
  , items    :: [RatingDescriptionItem]
} deriving (Show, Eq)

instance FromJSON AllRatingDescriptionQuery where
    parseJSON (Object o) =
      AllRatingDescriptionQuery <$> (o .: "sys")
                                <*> (o .: "total")
                                <*> (o .: "skip")
                                <*> (o .: "limit")
                                <*> (o .: "items")
    parseJSON _          = mzero

data RatingDescriptionItem = RatingDescriptionItem {
  sys :: SysItem
  , fields :: RatingDescriptionField
} deriving (Show, Eq)

instance FromJSON RatingDescriptionItem where
    parseJSON (Object o) = RatingDescriptionItem <$> (o .: "sys") <*> (o .: "fields")
    parseJSON _          = mzero

data RatingDescriptionField = RatingDescriptionField {
  name :: String
  , ratingSystem :: SysLink
  , description :: String
} deriving (Show,Eq)

instance FromJSON RatingDescriptionField where
    parseJSON (Object o) = 
        RatingDescriptionField <$> (o .: "name") 
                               <*> (o .: "ratingSystem")
                               <*> (o .: "description")
    parseJSON _          = mzero