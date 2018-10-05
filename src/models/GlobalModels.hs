{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Models.GlobalModels where

import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format

import GHC.Generics

type MString = Maybe String
type MInteger = Maybe Integer

data AllContentfulQuery a = AllContentfulQuery {
    sys :: SysLink
  , total    :: Integer
  , skip     :: Integer
  , limit    :: Integer
  , items    :: [ContentfulItem a]
} deriving (Show, Eq, Generic, FromJSON)

type MSysLink = Maybe SysLink
data SysLink = SysLink {
        _type     :: MString,
        linkType  :: MString,
        id        :: MString
} deriving (Show, Eq, Generic)

instance FromJSON SysLink where
    parseJSON = withObject "sys" $ \o -> do
        _type    <- o .:? "type"
        linkType <- o .:? "linkType"
        id       <- o .:? "id"
        return SysLink{..}

data SysItem = SysItem {
    space :: MSysLink
  , _type :: String
  , id   :: String
  , contentType :: SysLink
  , revision :: Integer
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , environment :: MSysLink
  , locale :: String
} deriving (Show, Eq, Generic)

instance FromJSON SysItem where
    parseJSON (Object o) =
        SysItem <$> ((o .: "space") >>= (.: "sys"))
                <*> (o .: "type")
                <*> (o .: "id")
                <*> ((o .: "contentType") >>= (.: "sys"))
                <*> (o .: "revision")
                <*> (o .: "createdAt")
                <*> (o .: "updatedAt")
                <*> ((o .: "environment") >>= (.: "sys"))
                <*> (o .: "locale")
    parseJSON _          = mzero

data ContentfulItem a = ContentfulItem {
    sys :: SysItem
    , fields :: a
} deriving (Show, Eq, Generic, FromJSON)

-- parseContentfulTime :: String -> UTCTime
-- parseContentfulTime t =
--     case parseTimeM True defaultTimeLocale "%F" t of
--     Just d -> d
--     Nothing -> error "could not parse date"

