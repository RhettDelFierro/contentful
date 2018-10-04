{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Models.GlobalModels where

import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format

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

data SysLink = SysLink {
        type_     :: Maybe String,
        linkType  :: Maybe String,
        id        :: Maybe String
} deriving (Show, Eq)

-- a is the id
data LinkType a = Space a | Environment a | ContentType a | Asset a | Entry a deriving (Show, Eq)

convertLinkType :: Maybe SysLink -> (Maybe (LinkType String))
convertLinkType (Just (SysLink (Just "Link") (Just "Environment") (Just id) )) = Just $ Environment id
convertLinkType (Just (SysLink (Just "Link") (Just "ContentType") (Just id))) = Just $ ContentType id
convertLinkType (Just (SysLink (Just "Link") (Just "Asset") (Just id))) = Just $ Asset id
convertLinkType (Just (SysLink (Just "Link") (Just "Entry") (Just id))) = Just $ Entry id
convertLinkType (Just (SysLink (Just "Link") (Just "Space") (Just id))) = Just $ Space id
convertLinkType _ = Nothing


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
