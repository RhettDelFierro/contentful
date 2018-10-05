{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Models.GlobalModels where

import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format

type MString = Maybe String

data AllContentfulQuery a = AllContentfulQuery {
    topSys :: SysLink
  , total    :: Integer
  , skip     :: Integer
  , limit    :: Integer
  , items    :: [a]
} deriving (Show, Eq)

instance (FromJSON a) => FromJSON (AllContentfulQuery a) where
    parseJSON (Object o) = 
        AllContentfulQuery <$> (o .: "sys")
                           <*> (o .: "total")
                           <*> (o .: "skip")
                           <*> (o .: "limit")
                           <*> (o .: "items")

type MSysLink = Maybe SysLink
data SysLink = SysLink {
        type_     :: MString,
        linkType  :: MString,
        id        :: MString
} deriving (Show, Eq)

data SysItem = SysItem {
  space :: MSysLink
  , sysItemType :: String
  , sysItemID   :: String
  , contentType :: SysLink
  , revision :: Integer
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , environment :: MSysLink
  , locale :: String
} deriving (Show, Eq)

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

instance FromJSON SysLink where
    parseJSON = withObject "sys" $ \o -> do
        type_    <- o .:? "type"
        linkType <- o .:? "linkType"
        id       <- o .:? "id"
        return SysLink{..}

-- parseContentfulTime :: String -> UTCTime
-- parseContentfulTime t =
--     case parseTimeM True defaultTimeLocale "%F" t of
--     Just d -> d
--     Nothing -> error "could not parse date"
