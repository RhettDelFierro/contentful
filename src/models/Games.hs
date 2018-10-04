{-# LANGUAGE OverloadedStrings #-}

module Models.Games where

import Models.GlobalModels (SysItem, SysLink)
import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format

data AllGamesQuery = AllGamesQuery {
  topGamesSys :: SysLink
  , total    :: Integer
  , skip     :: Integer
  , limit    :: Integer
  , items    :: [GameItem]
} deriving (Show, Eq)

instance FromJSON AllGamesQuery where
    parseJSON (Object o) =
      AllGamesQuery <$> (o .: "sys")
                       <*> (o .: "total")
                       <*> (o .: "skip")
                       <*> (o .: "limit")
                       <*> (o .: "items")
    parseJSON _          = mzero

data GameItem = GameItem {
  gameItemSys :: SysItem
  , gameItemFields :: GameField
} deriving (Show, Eq)

instance FromJSON GameItem where
    parseJSON (Object o) = GameItem <$> (o .: "sys") <*> (o .: "fields")
    parseJSON _          = mzero

data GameField = GameField {
  title :: String
  , launcherBackground :: SysLink
  , description :: String
  , exploreMoreLink :: String
  , numberOfPlayers :: Integer
  , supportedLanguages :: [String]
  , platforms :: [SysLink]
  , ratings :: [SysLink]
  , ratingDescriptions :: [SysLink]
  , minimumHardwareSpecifications :: SysLink
  , launcherIcon :: SysLink
  , launcherLogo :: SysLink
  , patchNotesImage :: SysLink
  , launcherFilters :: [SysLink]
  , orderLink :: String
  , launchInstallBackground :: SysLink
  , launchInstallHero :: SysLink
  , launchInstallLogo :: SysLink
  , launcherUuid :: String
  , visibleIfUnowned :: Bool
  , restrictedSettings :: SysLink
  , boxArt :: SysLink
} deriving (Show,Eq)

instance FromJSON GameField where
    parseJSON (Object o) = 
        GameField <$> (o .: "title") 
                      <*> (o .: "launcherBackground")
                      <*> (o .: "description")
                      <*> (o .: "exploreMoreLink")
                      <*> (o .: "numberOfPlayers")
                      <*> (o .: "supportedLanguages")
                      <*> (o .: "platforms")
                      <*> (o .: "ratings")
                      <*> (o .: "ratingDescriptions")
                      <*> (o .: "minimumHardwareSpecifications")
                      <*> (o .: "launcherIcon")
                      <*> (o .: "launcherLogo")
                      <*> (o .: "patchNotesImage")
                      <*> (o .: "launcherFilters")
                      <*> (o .: "orderLink")
                      <*> (o .: "launchInstallBackground")
                      <*> (o .: "launchInstallHero")
                      <*> (o .: "launchInstallLogo")
                      <*> (o .: "launcherUuid")
                      <*> (o .: "visibleIfUnowned")
                      <*> (o .: "restrictedSettings")
                      <*> (o .: "boxArt")
    parseJSON _          = mzero