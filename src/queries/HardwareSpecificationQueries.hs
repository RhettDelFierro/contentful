{-# LANGUAGE OverloadedStrings #-}
module Queries.HardwareSpecificationQueries(
    getAllHardwareSpecificationIO
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


import Models.HardwareSpecification

import Queries.GameQueries(getAllGameIO)

data EnvironmentConfig = EnvironmentConfig {
      preview_access_token_prod :: EnvironmentValue
    , space_contentful_launcher_prod :: EnvironmentValue
    , preview_access_token_sandbox :: EnvironmentValue
    , space_contentful_launcher_sandbox :: EnvironmentValue
}
type EnvironmentValue = String

makeUrlFromSpace :: Request
makeUrlFromSpace = "GET https://preview.contentful.com/spaces/52kyweqkx3gp/environments/master/entries?"

buildQueryHardwareSpecification :: ByteString -> [(ByteString, Maybe ByteString)]
buildQueryHardwareSpecification token = [("access_token", Just token), ("content_type", Just "hardwareSpecification")]

getEnvironmentVars :: IO EnvironmentConfig
getEnvironmentVars = do 
    token_prod <- getEnv "PREVIEW_ACCESS_TOKEN_PROD"
    space_prod <- getEnv "SPACE_CONTENTFUL_LAUNCHER_PROD"
    token_sandbox <- getEnv "PREVIEW_ACCESS_TOKEN"
    space_sandbox <- getEnv "SPACE_CONTENTFUL"
    return $ EnvironmentConfig token_prod space_prod token_sandbox space_sandbox

getHardwareSpecificationAPI :: [(ByteString, Maybe ByteString)] -> IO AllHardwareSpecificationQuery
getHardwareSpecificationAPI query = do
    let request = setQueryString query makeUrlFromSpace
    response <- httpJSON request
    return $ getResponseBody response

-- top level interface
getAllHardwareSpecificationIO :: IO [HardwareSpecificationItem]
getAllHardwareSpecificationIO = do
    config <- getEnvironmentVars
    hws <- getHardwareSpecificationAPI $ buildQueryHardwareSpecification $ fromString $ preview_access_token_sandbox config
    return $ items hws

-- findMatches :: IO [a]
-- findMatches = do
--     hwis <- getAllHardwareSpecificationIO
--     gs <- getAllGameIO
--     undefined


-- printEach :: IO ()
-- printEach = do
--     hwis <- buildQueryIO
--     let hfs = hardwareItemFields <$> hwis
--         titles = title <$> hfs
--         ps = print <$> titles
--     return ()