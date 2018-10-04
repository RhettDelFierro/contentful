{-# LANGUAGE OverloadedStrings #-}
module Models.GlobalModels where

import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format

data SysLink = SysLink {
        sysLinkType     :: String,
        sysLinkLinkType :: String,
        sysLinkID       :: String
} deriving (Show, Eq)

data SysItem = SysItem {
  space :: SysLink
  , sysItemType :: String
  , sysItemID   :: String
  , contentType :: SysLink
  , revision :: Integer
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , environment :: SysLink
  , locale :: String
} deriving (Show, Eq)

instance FromJSON SysItem where
    parseJSON (Object o) =
        SysItem <$> ((o .: "space") >>= (.: "sys"))
                <*> (o .: "type")
                <*> (o .: "id")
                <*> ((o .: "contentType") >>= (.: "sys"))
                <*> (o .: "revision")
                <*> (parseContentfulTime <$> o .: "createdAt")
                <*> (parseContentfulTime <$> o .: "updatedAt")
                <*> ((o .: "environment") >>= (.: "sys"))
                <*> (o .: "locale")
    parseJSON _          = mzero

instance FromJSON SysLink where
    parseJSON (Object o) =
        SysLink <$> ((o .: "sys") >>= (.: "type"))
                <*> ((o .: "sys") >>= (.: "linkType"))
                <*> ((o .: "sys") >>= (.: "id"))
    parseJSON _          = mzero

    
parseContentfulTime :: String -> UTCTime
parseContentfulTime t =
    case parseTimeM True defaultTimeLocale "%F" t of
    Just d -> d
    Nothing -> error "could not parse date"
