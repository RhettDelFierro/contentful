{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
  title :: Maybe String
  , cpu :: Maybe String
  , os :: Maybe String
  , gpu :: Maybe String
  , freeDiskSpace :: Maybe Integer
  , osBit :: Maybe Integer
  , cpuLogicalCores :: Maybe Integer
  , cpuMemory :: Maybe Integer
  , cpuPowerIntel :: Maybe Integer
  , cpuPowerAmd :: Maybe Integer
  , gpuMemoryNvidia :: Maybe Integer
  , gpuPowerNvidia :: Maybe Integer
  , gpuMemoryAmd :: Maybe Integer
  , gpuPowerAmd :: Maybe Integer
  , gpuMemoryIntel :: Maybe Integer
  , gpuPowerIntel :: Maybe Integer
} deriving (Show,Eq)

instance FromJSON HardwareSpecificationField where
    parseJSON (Object o) = 
        HardwareSpecificationField <$> (o .:? "title") 
                      <*> (o .:? "cpu")
                      <*> (o .:? "os")
                      <*> (o .:? "gpu")
                      <*> (o .:? "freeDiskSpace")
                      <*> (o .:? "osBit")
                      <*> (o .:? "cpuLogicalCores")
                      <*> (o .:? "cpuMemory")
                      <*> (o .:? "cpuPowerIntel")
                      <*> (o .:? "cpuPowerAmd")
                      <*> (o .:? "gpuMemoryNvidia")
                      <*> (o .:? "gpuPowerNvidia")
                      <*> (o .:? "gpuMemoryAmd")
                      <*> (o .:? "gpuPowerAmd")
                      <*> (o .:? "gpuMemoryIntel")
                      <*> (o .:?  "gpuPowerIntel")
    parseJSON _          = mzero

-- instance FromJSON HardwareSpecificationField where
--     parseJSON (Object o) = do
--         title           <- o .: "title"
--         cpu             <- o .: "cpu"
--         os              <- o .: "os"
--         gpu             <- o .: "gpu"
--         freeDiskSpace   <- o .: "freeDiskSpace"
--         osBit           <- o .: "osBit"
--         cpuLogicalCores <- o .: "cpuLogicalCores"
--         cpuMemory       <- o .:? "cpuMemory"
--         cpuPowerIntel   <- o .:? "cpuPowerIntel"
--         cpuPowerAmd   <- o .:? "cpuPowerAmd"
--         gpuMemoryNvidia <- o .:? "gpuMemoryNvidia"
--         gpuPowerNvidia  <- o .:? "gpuPowerNvidia"
--         gpuMemoryAmd <- o .:? "gpuMemoryAmd"
--         gpuPowerAmd  <- o .:? "gpuPowerAmd"
--         gpuMemoryIntel <- o .:? "gpuMemoryIntel"
--         gpuPowerIntel  <- o .:?  "gpuPowerIntel"
--         return HardwareSpecificationField{..}
 