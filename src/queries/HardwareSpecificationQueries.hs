{-# LANGUAGE OverloadedStrings #-}
module Queries.HardwareSpecificationQueries where

import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Client (setQueryString)
import Data.ByteString.Base64
import Data.ByteString.UTF8 hiding (foldl)
import Data.Foldable (foldl)
import Data.Monoid
import Control.Applicative

import Global.GlobalSys
import Models.GlobalModels
import Models.HardwareSpecification

makeUrlFromSpace :: Request
makeUrlFromSpace = "GET https://preview.contentful.com/spaces/52kyweqkx3gp/environments/master/entries?"

buildQueryHardwareSpecification :: ByteString -> [(ByteString, Maybe ByteString)]
buildQueryHardwareSpecification token = [("access_token", Just token), ("content_type", Just "hardwareSpecification")]

getHardwareSpecificationAPI :: [(ByteString, Maybe ByteString)] -> IO (AllContentfulQuery HardwareSpecificationField)
getHardwareSpecificationAPI query = do
    let request = setQueryString query makeUrlFromSpace
    response <- httpJSON request
    return $ getResponseBody response

getAllHardwareSpecificationIO :: IO [HardwareSpecificationField]
getAllHardwareSpecificationIO = do
    config <- getEnvironmentVars
    hws <- getHardwareSpecificationAPI $ buildQueryHardwareSpecification $ fromString $ preview_access_token_sandbox config
    return $ fields <$> items hws

-- getAllHardwareSpecificationIO :: IO [Maybe Integer]
-- getAllHardwareSpecificationIO = do
--     config <- getEnvironmentVars
--     hws <- getHardwareSpecificationAPI $ buildQueryHardwareSpecification $ fromString $ preview_access_token_sandbox config
--     return $ map cpuPowerIntel $ fields <$> items hws
    -- undefined
-- top level interface


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