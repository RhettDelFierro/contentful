{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.HardwareSpecification where

import Models.GlobalModels (MInteger, MString, SysItem, SysLink)
import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format
import GHC.Generics

data HardwareSpecificationField = HardwareSpecificationField {
    title :: MString
  , cpu :: MString
  , os :: MString
  , gpu :: MString
  , freeDiskSpace :: MInteger
  , osBit :: MInteger
  , cpuLogicalCores :: MInteger
  , cpuMemory :: MInteger
  , cpuPowerIntel :: MInteger
  , cpuPowerAmd :: MInteger
  , gpuMemoryNvidia :: MInteger
  , gpuPowerNvidia :: MInteger
  , gpuMemoryAmd :: MInteger
  , gpuPowerAmd :: MInteger
  , gpuMemoryIntel :: MInteger
  , gpuPowerIntel :: MInteger
} deriving (Show,Eq, Generic, FromJSON)

-- instance FromJSON HardwareSpecificationField where
--     parseJSON (Object o) = 
--         HardwareSpecificationField <$> (o .:? "title") 
--                       <*> (o .:? "cpu")
--                       <*> (o .:? "os")
--                       <*> (o .:? "gpu")
--                       <*> (o .:? "freeDiskSpace")
--                       <*> (o .:? "osBit")
--                       <*> (o .:? "cpuLogicalCores")
--                       <*> (o .:? "cpuMemory")
--                       <*> (o .:? "cpuPowerIntel")
--                       <*> (o .:? "cpuPowerAmd")
--                       <*> (o .:? "gpuMemoryNvidia")
--                       <*> (o .:? "gpuPowerNvidia")
--                       <*> (o .:? "gpuMemoryAmd")
--                       <*> (o .:? "gpuPowerAmd")
--                       <*> (o .:? "gpuMemoryIntel")
--                       <*> (o .:?  "gpuPowerIntel")
--     parseJSON _          = mzero

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
 