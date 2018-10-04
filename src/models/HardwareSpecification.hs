{-# LANGUAGE OverloadedStrings #-}

module Models.HardwareSpecification where

import Models.GlobalModels (SysItem, SysLink)
import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format


data HardwareSpecificationItem = HardwareSpecificationItem {
  sys :: SysItem
  , fields :: HardwareSpecificationField
} deriving (Show, Eq)

instance FromJSON HardwareSpecificationItem where
    parseJSON (Object o) = HardwareSpecificationItem <$> (o .: "sys") <*> (o .: "fields")
    parseJSON _          = mzero

data HardwareSpecificationField = HardwareSpecificationField {
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

instance FromJSON HardwareSpecificationField where
    parseJSON (Object o) = 
        HardwareSpecificationField <$> (o .: "title") 
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