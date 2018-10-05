{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Game where

import Models.GlobalModels (MString, MSysLink, SysItem)
import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format
import GHC.Generics

data GameItem = GameItem {
  sys :: SysItem
  , fields :: GameField
} deriving (Show, Eq)

instance FromJSON GameItem where
    parseJSON (Object o) = GameItem <$> (o .: "sys") <*> (o .: "fields")
    parseJSON _          = mzero

data GameField = GameField {
    title :: MString
  , launcherBackground  :: MSysLink
  , description         :: MString
  , exploreMoreLink     :: MString
  , numberOfPlayers     :: MString
  , supportedLanguages  :: Maybe [String]
  , platforms           :: Maybe [MSysLink]
  , ratings             :: Maybe [MSysLink]
  , ratingDescriptions  :: Maybe [MSysLink]
  , minimumHardwareSpecifications :: MSysLink
  , launcherIcon        :: MSysLink
  , launcherLogo        :: MSysLink
  , patchNotesImage     :: MSysLink
  , launcherFilters     :: Maybe [MSysLink]
  , orderLink           :: MString
  , launchInstallBackground :: MSysLink
  , launchInstallHero   :: MSysLink
  , launchInstallLogo   :: MSysLink
  , launcherUuid        :: MString
  , visibleIfUnowned    :: Maybe Bool
  , restrictedSettings  :: MSysLink
  , boxArt              :: MSysLink
} deriving (Show,Eq, Generic)

instance FromJSON GameField where