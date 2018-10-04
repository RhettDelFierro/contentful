{-# LANGUAGE OverloadedStrings #-}

module Models.Hardware where

import Models.GlobalModels (SysItem, SysLink)
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