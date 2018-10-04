{-# LANGUAGE OverloadedStrings #-}

module Models.Asset where

import Models.GlobalModels (SysItem, SysLink)
import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format

data AllAssetQuery = AllAssetQuery {
  topSys :: SysLink
  , total    :: Integer
  , skip     :: Integer
  , limit    :: Integer
  , items    :: [AssetItem]
} deriving (Show, Eq)

instance FromJSON AllAssetQuery where
    parseJSON (Object o) =
      AllAssetQuery <$> (o .: "sys")
                        <*> (o .: "total")
                        <*> (o .: "skip")
                        <*> (o .: "limit")
                        <*> (o .: "items")
    parseJSON _          = mzero

data AssetItem = AssetItem {
  sys :: SysItem
  , fields :: AssetField
} deriving (Show, Eq)

instance FromJSON AssetItem where
    parseJSON (Object o) = AssetItem <$> (o .: "sys") <*> (o .: "fields")
    parseJSON _          = mzero

data AssetField = AssetField {
  title :: String
  , file :: AssetFile
} deriving (Show, Eq)

instance FromJSON AssetField where
    parseJSON (Object o) = 
        AssetField <$> (o .: "title") 
                   <*> (o .: "file")
    parseJSON _          = mzero

data AssetFile = AssetFile {
        url :: String
        , details :: AssetFileDetails
        , fileName :: String
        , contentType :: String
} deriving (Show, Eq)

instance FromJSON AssetFile where
    parseJSON (Object o) = 
        AssetFile <$> (o .: "url") 
                  <*> (o .: "details")
                  <*> (o .: "fileName")
                  <*> (o .: "contentType")
    parseJSON _          = mzero

data AssetFileDetails = AssetFileDetails {
        size :: Integer
        , image :: AssetFileDetailsImage
} deriving (Show, Eq)

instance FromJSON AssetFileDetails where
    parseJSON (Object o) = 
        AssetFileDetails <$> (o .: "size") 
                         <*> (o .: "image")
    parseJSON _          = mzero

data AssetFileDetailsImage = AssetFileDetailsImage {
    width :: Integer
    , height :: Integer
} deriving (Show, Eq)

instance FromJSON AssetFileDetailsImage where
    parseJSON (Object o) = 
        AssetFileDetailsImage <$> (o .: "width") 
                         <*> (o .: "height")
    parseJSON _          = mzero
