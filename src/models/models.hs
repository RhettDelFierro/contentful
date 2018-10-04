{-# LANGUAGE OverloadedStrings #-}
module Models where

import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format

data AllHardwareQuery = AllHardwareQuery {
  topHardwareSys :: SysLink
  , total    :: Integer
  , skip     :: Integer
  , limit    :: Integer
  , items    :: [HardwareItem]
} deriving (Show, Eq)

instance FromJSON AllHardwareQuery where
    parseJSON (Object o) =
      AllHardwareQuery <$> (o .: "sys")
                       <*> (o .: "total")
                       <*> (o .: "skip")
                       <*> (o .: "limit")
                       <*> (o .: "items")
    parseJSON _          = mzero

data HardwareItem = HardwareItem {
  hardwareSys :: SysItem
  , hardwareItemFields :: HardwareField
} deriving (Show, Eq)


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
                <*> (parseHardwareTime <$> o .: "createdAt")
                <*> (parseHardwareTime <$> o .: "updatedAt")
                <*> ((o .: "environment") >>= (.: "sys"))
                <*> (o .: "locale")
    parseJSON _          = mzero

instance FromJSON SysLink where
    parseJSON (Object o) =
        SysLink <$> ((o .: "sys") >>= (.: "type"))
                <*> ((o .: "sys") >>= (.: "linkType"))
                <*> ((o .: "sys") >>= (.: "id"))
    parseJSON _          = mzero

instance FromJSON HardwareItem where
    parseJSON (Object o) = HardwareItem <$> (o .: "sys") <*> (o .: "fields")
    parseJSON _          = mzero

data HardwareField = HardwareField {
  title :: String
  , cpu :: String
  , os :: String
  , gpu :: String
  , freeDiskSpace :: Integer
  , osBit :: Integer
  , cpuLogicalCores :: Integer
  , cpuMemory :: Integer
  , cpuPowerIntel :: Integer
  , cpuPowerAmd :: Integer
  , gpuMemoryNvidia :: Integer
  , gpuPowerNvidia :: Integer
  , gpuMemoryAmd :: Integer
  , gpuPowerAmd :: Integer
  , gpuMemoryIntel :: Integer
  , gpuPowerIntel :: Integer
} deriving (Show,Eq)

instance FromJSON HardwareField where
    parseJSON (Object o) = 
        HardwareField <$> (o .: "title") 
                      <*> (o .: "cpu")
                      <*> (o .: "os")
                      <*> (o .: "gpu")
                      <*> (o .: "freeDiskSpace")
                      <*> (o .: "osBit")
                      <*> (o .: "cpuLogicalCores")
                      <*> (o .: "cpuMemory")
                      <*> (o .: "cpuPowerIntel")
                      <*> (o .: "cpuPowerAmd")
                      <*> (o .: "gpuMemoryNvidia")
                      <*> (o .: "gpuPowerNvidia")
                      <*> (o .: "gpuMemoryAmd")
                      <*> (o .: "gpuPowerAmd")
                      <*> (o .: "gpuMemoryIntel")
                      <*> (o .: "gpuPowerIntel")
    parseJSON _          = mzero

parseHardwareTime :: String -> UTCTime
parseHardwareTime t =
    case parseTimeM True defaultTimeLocale "%F" t of
    Just d -> d
    Nothing -> error "could not parse date"
