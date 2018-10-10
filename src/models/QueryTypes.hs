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
                      } deriving (Show,Eq, Generic, FromJSON)

data GameRestrictedField = 
    GameRestrictedField { visibleIfUnowned :: Maybe Bool
                        , uuid :: MString
                        , favoriteOnDiscovery :: Maybe Bool
                        } deriving (Show,Eq, Generic, FromJSON)
  
data GenreField = GenreField { name :: MString } deriving (Show, Eq, Generic, FromJSON)
  
data HardwareDataField = 
    HardwareDataField { name :: MString
                      , _type :: MSysLink
                      , manufacturer :: MSysLink
                      , family :: MString
                      , familyClassification :: MSysLink
                      , model :: MString
                      , modifier :: MString
                      , frequency :: MInteger
                      , performance :: MInteger
                      } deriving (Show, Eq, Generic, FromJSON)

data HardwareFamilyField = HardwareFamilyField { name :: MString } deriving (Show, Eq, Generic, FromJSON)

data HardwareFamilyClassifierField =
    HardwarefamilyClassifierField { name :: MString
                                  , searchText :: MString
                                  , _type :: MSysLink
                                  , manufacturer :: MSysLink
                                  , family :: MSysLink
                                  , modifierBeginning :: Maybe Bool
                                  } deriving (Show, Eq, Generic, FromJSON)

data HardwareManufacturerField = HardwareManufacturerField { name :: MString } deriving (Show, Eq, Generic, FromJSON)
  
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
  
data HardwareTypeField = HardwareTypeField { name :: MString } deriving (Show, Eq, Generic, FromJSON)

data LinkField = LinkField { linkDisplayName :: MString
                           , linkUrl :: MString
                           } deriving (Show, Eq, Generic, FromJSON)

data LauncherConfigField = 
    LauncherConfigField { name :: MString
                        , releasteNotes :: Maybe [MSysLink]
                        , loginBackground :: MSysLink
                        , translations :: Maybe Object
                        , minimumHardwareSpecificationDatabase :: Maybe [MSysLink]
                        , launchInstallLogo :: MSysLink
                        , hardwareFamilyDatabase :: Maybe [MSysLink]
                        , paymentProcessors :: Maybe [MSysLink]
                        , germanAgegate :: Maybe [MSysLink]
                        , gamePageRepeatBackground :: MSysLink
                        , favoriteGameListOrder :: Maybe [MSysLink]
                        , promoModals :: Maybe [MSysLink]
                        , externalLInks :: MSysLink
                        , developerModePage :: MSysLink
                        } deriving (Show, Eq, Generic, FromJSON)

data LauncherExternalLinksField =
    LauncherExternalLinksField { name :: MString
                               , accountManagementUrlTest :: MString
                               , accountManagementUrlIntegration :: MString
                               , accountManagementUrlStaging :: MString
                               , accountManagementUrlProduction :: MString
                               , communityUrlTest :: MString
                               , communityUrlIntegration :: MString
                               , communityUrlStaging :: MString
                               , communityUrlProduction :: MString
                               , createAccountUrlTest :: MString
                               , createAccountUrlIntegration :: MString
                               , createAccountUrlStaging :: MString
                               , createAccountUrlProduction :: MString
                               , modsUrlTest :: MString
                               , modsUrlIntegration :: MString
                               , modsUrlStaging :: MString
                               , modsUrlProduction :: MString
                               , newsUrlTest :: MString
                               , newsUrlIntegration :: MString
                               , newsUrlStaging :: MString
                               , newsUrlProduction :: MString
                               , shopUrlTest :: MString
                               , shopUrlIntegration :: MString
                               , shopUrlStaging :: MString
                               , shopUrlProduction :: MString
                               , streamsUrlTest :: MString
                               , streamsUrlIntegration :: MString
                               , streamsUrlStaging :: MString
                               , streamsUrlProduction :: MString
                               , visitSupportUrlTest :: MString
                               , visitSupportUrlIntegration :: MString
                               , visitSupportUrlStaging :: MString
                               , visitSupportUrlProduction :: MString
                               , forgotUsernameUrlTest :: MString
                               , forgotUsernameUrlIntegration :: MString
                               , forgotUsernameUrlStaging :: MString
                               , forgotUsernameUrlProduction :: MString
                               , forgotPasswordUrlTest :: MString
                               , forgotPasswordUrlIntegration :: MString
                               , forgotPasswordUrlStaging :: MString
                               , forgotPasswordUrlProduction :: MString
                               } deriving (Show, Eq, Generic, FromJSON)

data LauncherFilterField = 
    LauncherFilterField { name ::MString
                        , displayName :: MString
                        } deriving (Show, Eq, Generic, FromJSON)

data LauncherSidebarLinksField =
    LauncherSidebarLinksField { name :: MString
                              , officialSite :: MString
                              , twitter :: MString
                              , facebook :: MString
                              , forums :: MString
                              , instagram :: MString
                              , youtube :: MString
                              , reddit :: MString
                              , customLinkDisplayName1 :: MString
                              , customLinkDisplayName2 :: MString
                              , customLinkDisplayName3 :: MString
                              , customLinkDisplayName4 :: MString
                              , customLinkUrl1 :: MString
                              , customLinkUrl2 :: MString
                              , customLinkUrl3 :: MString
                              , customLinkUrl4 :: MString
                              } deriving (Show, Eq, Generic, FromJSON)
data MessageOfTheDayField =
    MessageOfTheDayField { entriTitle :: MString
                         , title :: MString
                         , body :: MString
                         } deriving (Show, Eq, Generic, FromJSON)
data PatchNoteField = 
    PatchNoteField { title :: MString
                    , body :: MString
                    , version :: MString
                    } deriving (Show, Eq, Generic, FromJSON)
  
data PaymentProcessorField =
    PaymentProcessorField { displayName :: MString
                          , _type :: MString
                          , image :: MSysLink
                          } deriving (Show, Eq, Generic, FromJSON)

data PromoModalField =
    PromoModalField { promoTitle :: MString
                    , primaryImage :: MSysLink
                    , buttons :: Maybe [MSysLink]
                    } deriving (Show, Eq, Generic, FromJSON)

data PromoModalButtonField =
    PromoModalButtonField { buttonName :: MString
                          , buttonFunction :: MString
                          , externalUrl :: MString
                          , buttonText :: MString
                          , customText :: MString
                          , game :: MSysLink
    } deriving (Show, Eq, Generic, FromJSON)

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

data RatingSystemField =
    RatingSystemField { name :: MString
                      , supportedCountries :: Maybe [String]
                      } deriving (Show, Eq, Generic, FromJSON)

data ReleaseNoteField =
    ReleaseNoteField { patchReleaseDate :: MString
                     , clientVersion :: MString
                     , uiVersion :: MString
                     , newFeatureList :: Maybe [String]
                     , improvementsContent :: MString
                     , improvementsList :: Maybe [String]
                     , bugFixesList :: Maybe [String]
                     } deriving (Show, Eq, Generic, FromJSON)

data SkuField = 
    SkuField { title :: MString
             , referenceId :: MString
             , customBackgroundImage :: MSysLink
             , features :: Maybe [MSysLink]
             , shortDescription :: MString
             } deriving (Show,Eq, Generic, FromJSON)

data SkuFeatureField =
    SkuFeatureField { name :: MString
                    , attribute :: Maybe [String]
                    } deriving (Show, Eq, Generic, FromJSON)