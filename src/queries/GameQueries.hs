{-# LANGUAGE OverloadedStrings #-}
module Queries.GameQueries(
    getAllGameIO
) where

import Control.Concurrent.Async(mapConcurrently)
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Client (setQueryString)
import Data.ByteString.Base64
import Data.ByteString.UTF8 hiding (foldl)
import Data.Foldable (foldl)
import Data.Monoid
import System.Environment (getEnv)
import Control.Applicative

import Models.GlobalModels
import Models.Game

data EnvironmentConfig = EnvironmentConfig {
      preview_access_token_prod :: EnvironmentValue
    , space_contentful_launcher_prod :: EnvironmentValue
    , preview_access_token_sandbox :: EnvironmentValue
    , space_contentful_launcher_sandbox :: EnvironmentValue
}
type EnvironmentValue = String

allLocales :: [ByteString]
allLocales = map fromString ["en", "fr", "it", "de", "es", "pl", "ru", "pt", "es-419", "ja", "zh-CN", "zh-TW", "ko", "tr"]

makeUrlFromSpace :: Request --sandbox
makeUrlFromSpace = "GET https://preview.contentful.com/spaces/52kyweqkx3gp/environments/master/entries?"

buildQueryGame :: ByteString -> [(ByteString, Maybe ByteString)]
buildQueryGame token = [("access_token", Just token), ("content_type", Just "game")]

buildQueryGameLocales :: ByteString -> ByteString -> [(ByteString, Maybe ByteString)]
buildQueryGameLocales token locale = [("access_token", Just token), ("content_type", Just "game"), ("content_type", Just locale)]

getEnvironmentVars :: IO EnvironmentConfig
getEnvironmentVars = do 
    token_prod <- getEnv "PREVIEW_ACCESS_TOKEN_PROD"
    space_prod <- getEnv "SPACE_CONTENTFUL_LAUNCHER_PROD"
    token_sandbox <- getEnv "PREVIEW_ACCESS_TOKEN"
    space_sandbox <- getEnv "SPACE_CONTENTFUL"
    return $ EnvironmentConfig token_prod space_prod token_sandbox space_sandbox

getGameAPI :: [(ByteString, Maybe ByteString)] -> IO (AllContentfulQuery AllGameQuery)
getGameAPI query = do
    let request = setQueryString query makeUrlFromSpace
    response <- httpJSON request
    return $ getResponseBody response


getAllGameIO :: IO (AllContentfulQuery AllGameQuery)
getAllGameIO = do
    config <- getEnvironmentVars
    getGameAPI $ buildQueryGame $ fromString $ preview_access_token_sandbox config
    -- undefined
    -- return $ items <$> gs


-- top level interface
-- getAllGameIO :: IO [GameItem]
-- getAllGameIO = do
--     config <- getEnvironmentVars
--     gs <- getGameAPI $ buildQueryGame $ fromString $ preview_access_token_sandbox config
--     return $ items gs

-- getMultipleGameIO :: IO [[GameItem]]
-- getMultipleGameIO = do
--     config <- getEnvironmentVars
--     gs <- mapConcurrently getGameAPI $ buildQueryGameLocales (fromString (preview_access_token_sandbox config)) <$> allLocales
--     let a = items <$> gs -- :: [[GameItem]]
--     undefined

-- data TranslationNeeded a = TranslationNeeded {
--     locale :: String
--   , translationNeeded :: a 
-- }

-- checkField :: String -> GameItem -> Maybe TranslationNeeded a
-- checkField lang itm = 

-- findNoField :: [[GameItem]] -> [Maybe [TranslationNeeded]]
-- findNoField [eng:gs] = map filterField gs
--   where filterField [others] = 