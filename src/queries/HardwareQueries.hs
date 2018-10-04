{-# LANGUAGE OverloadedStrings #-}
module Queries.HardwareQueries(
    getAllHardwareIO
) where

import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Client (setQueryString)
import Data.ByteString.Base64
import Data.ByteString.UTF8 hiding (foldl)
import Data.Foldable (foldl)
import Data.Monoid
import System.Environment (getEnv)
import Control.Applicative


import Models.Hardware

data EnvironmentConfig = EnvironmentConfig {
      preview_access_token_prod :: EnvironmentValue
    , space_contentful_launcher_prod :: EnvironmentValue
    , preview_access_token_sandbox :: EnvironmentValue
    , space_contentful_launcher_sandbox :: EnvironmentValue
}
type EnvironmentValue = String

makeUrlFromSpace :: Request
makeUrlFromSpace = "GET https://preview.contentful.com/spaces/52kyweqkx3gp/environments/master/entries?"

buildQueryHardware :: ByteString -> [(ByteString, Maybe ByteString)]
buildQueryHardware token = [("access_token", Just token), ("content_type", Just "hardwareSpecification")]

getEnvironmentVars :: IO EnvironmentConfig
getEnvironmentVars = do 
    token_prod <- getEnv "PREVIEW_ACCESS_TOKEN_PROD"
    space_prod <- getEnv "SPACE_CONTENTFUL_LAUNCHER_PROD"
    token_sandbox <- getEnv "PREVIEW_ACCESS_TOKEN"
    space_sandbox <- getEnv "SPACE_CONTENTFUL"
    return $ EnvironmentConfig token_prod space_prod token_sandbox space_sandbox

getHardwareAPI :: [(ByteString, Maybe ByteString)] -> IO AllHardwareQuery
getHardwareAPI query = do
    let request = setQueryString query makeUrlFromSpace
    response <- httpJSON request
    return $ getResponseBody response

-- top level interface
getAllHardwareIO :: IO [HardwareItem]
getAllHardwareIO = do
    config <- getEnvironmentVars
    hws <- getHardwareAPI $ buildQueryHardware $ fromString $ preview_access_token_sandbox config
    return $ items hws

-- printEach :: IO ()
-- printEach = do
--     hwis <- buildQueryIO
--     let hfs = hardwareItemFields <$> hwis
--         titles = title <$> hfs
--         ps = print <$> titles
--     return ()