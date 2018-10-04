{-# LANGUAGE OverloadedStrings #-}

module Models.Game where

import Models.GlobalModels (convertLinkType, SysItem, SysLink, LinkType)
import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format

data GameItem = GameItem {
  sys :: SysItem
  , fields :: GameField
} deriving (Show, Eq)

instance FromJSON GameItem where
    parseJSON (Object o) = GameItem <$> (o .: "sys") <*> (o .: "fields")
    parseJSON _          = mzero

data GameField = GameField {
  title :: Maybe String
  , launcherBackground :: Maybe (LinkType String)
  , description :: Maybe String
  , exploreMoreLink :: Maybe String
  , numberOfPlayers :: Maybe String
  , supportedLanguages :: Maybe [String]
  , platforms :: Maybe [SysLink]
  , ratings :: Maybe [SysLink]
  , ratingDescriptions :: Maybe [SysLink]
  , minimumHardwareSpecifications :: Maybe (LinkType String)
  , launcherIcon :: Maybe (LinkType String)
  , launcherLogo :: Maybe (LinkType String)
  , patchNotesImage :: Maybe SysLink
  , launcherFilters :: Maybe [SysLink]
  , orderLink :: Maybe String
  , launchInstallBackground :: Maybe (LinkType String)
  , launchInstallHero :: Maybe (LinkType String)
  , launchInstallLogo :: Maybe (LinkType String)
  , launcherUuid :: Maybe String
  , visibleIfUnowned :: Maybe Bool
  , restrictedSettings :: Maybe (LinkType String)
  , boxArt :: Maybe (LinkType String)
} deriving (Show,Eq)

instance FromJSON GameField where
    parseJSON (Object o) = 
        GameField <$> (o .:? "title") 
                      <*> (convertLinkType <$> o .:? "launcherBackground")
                      <*> (o .:? "description")
                      <*> (o .:? "exploreMoreLink")
                      <*> (o .:? "numberOfPlayers")
                      <*> (o .:? "supportedLanguages")
                      <*> (o .:? "platforms")
                      <*> (o .:? "ratings")
                      <*> (o .:? "ratingDescriptions")
                      <*> (convertLinkType <$> o .:? "minimumHardwareSpecifications")
                      <*> (convertLinkType <$> o .:? "launcherIcon")
                      <*> (convertLinkType <$> o .:? "launcherLogo")
                      <*> (o .:? "patchNotesImage")
                      <*> (o .:? "launcherFilters")
                      <*> (o .:? "orderLink")
                      <*> (convertLinkType <$> o .:? "launchInstallBackground")
                      <*> (convertLinkType <$> o .:? "launchInstallHero")
                      <*> (convertLinkType <$> o .:? "launchInstallLogo")
                      <*> (o .:? "launcherUuid")
                      <*> (o .:? "visibleIfUnowned")
                      <*> (convertLinkType <$> o .:? "restrictedSettings")
                      <*> (convertLinkType <$> o .:? "boxArt")
    parseJSON _          = mzero