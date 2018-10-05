{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.QueryTypes where

import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format

import GHC.Generics
import Models.GlobalModels (MString, MInteger, MSysLink)


data AssetFileDetailsImage = 
    AssetFileDetailsImage { width :: MInteger 
                          , height :: MInteger 
                          } deriving (Show, Eq, Generic, FromJSON)

data AssetFileDetails = 
    AssetFileDetails { size :: MInteger
                     , image :: Maybe AssetFileDetailsImage 
                     } deriving (Show, Eq, Generic, FromJSON)

data AssetFile = 
    AssetFile { url :: MString
              , details :: Maybe AssetFileDetails
              , fileName :: MString
              , contentType :: String
              } deriving (Show, Eq, Generic, FromJSON)

data AssetField = 
    AssetField { title :: MString 
               , file :: Maybe AssetFile 
               } deriving (Show, Eq, Generic, FromJSON)

data DeveloperField = 
    DeveloperField { entryTitle :: MString
                   , name :: MString
                   , logo :: MSysLink
                   } deriving (Show,Eq, Generic, FromJSON)

data GameField = 
    GameField { title :: MString
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
              } deriving (Show,Eq, Generic, FromJSON)
                            
data GamePlatformField = 
    GamePlatformField { entryTitle :: MString
                      , platform :: MSysLink
                      } deriving (Show,Eq)

data GameRestrictedField = 
    GameRestrictedField { visibleIfUnowned :: Maybe Bool
                        , uuid :: MString
                        , favoriteOnDiscovery :: Maybe Bool
                        } deriving (Show,Eq, Generic, FromJSON)

data HardwareSpecificationField = 
    HardwareSpecificationField { title :: MString
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

data PatchNoteField = 
    PatchNoteField { title :: MString
                   , body :: MString
                   , version :: MString
                   } deriving (Show,Eq, Generic, FromJSON)

data RatingField = 
    RatingField { name :: MString
                , ratingSystem :: MSysLink
                , shortName :: MString
                , ratingImage :: MSysLink
                , key :: MString
                } deriving (Show,Eq, Generic, FromJSON)

data RatingDescriptionField = 
    RatingDescriptionField { name :: MString
                           , ratingSystem :: MSysLink
                           , description :: MString
                           } deriving (Show,Eq, Generic, FromJSON)

data SkuField = 
    SkuField { title :: MString
             , referenceId :: MString
             , customBackgroundImage :: MSysLink
             , features :: Maybe [MSysLink]
             , shortDescription :: MString
             } deriving (Show,Eq, Generic, FromJSON)
                        
                        