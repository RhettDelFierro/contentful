{-# LANGUAGE OverloadedStrings #-}
module Queries.GameQueries where

import Control.Concurrent.Async(mapConcurrently)
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Client (setQueryString)
import Data.ByteString.Base64
import Data.ByteString.UTF8 hiding (foldl)
import Data.Foldable (foldl)
import Data.Monoid
import Control.Applicative

import Global.GlobalSys (getEnvironmentVars, preview_access_token_sandbox)
import Models.GlobalModels
import Models.QueryTypes

allLocales :: [ByteString]
allLocales = map fromString ["en", "fr", "it", "de", "es", "pl", "ru", "pt", "es-419", "ja", "zh-CN", "zh-TW", "ko", "tr"]

makeUrlFromSpace :: Request --sandbox
makeUrlFromSpace = "GET https://preview.contentful.com/spaces/52kyweqkx3gp/environments/master/entries?"

buildQueryGame :: ByteString -> [(ByteString, Maybe ByteString)]
buildQueryGame token = [("access_token", Just token), ("content_type", Just "game")]

buildQueryGameLocales :: ByteString -> ByteString -> [(ByteString, Maybe ByteString)]
buildQueryGameLocales token locale = [("access_token", Just token), ("content_type", Just "game"), ("content_type", Just locale)]

getGameAPI :: [(ByteString, Maybe ByteString)] -> IO (AllContentfulQuery GameField)
getGameAPI query = do
    let request = setQueryString query makeUrlFromSpace
    response <- httpJSON request
    return $ getResponseBody response

getAllGameIO :: IO [GameField]
getAllGameIO = do
    config <- getEnvironmentVars
    gs     <- getGameAPI $ buildQueryGame $ fromString $ preview_access_token_sandbox config
    return $ fields <$> items gs

getMultipleGameIO :: IO [[GameField]]
getMultipleGameIO = do
    config <- getEnvironmentVars
    gs     <- mapConcurrently getGameAPI $ buildQueryGameLocales (fromString (preview_access_token_sandbox config)) <$> allLocales
    -- return $ map fields $ items <$> gs
    undefined

data TranslationNeeded a = TranslationNeeded {
    locale :: String
  , translationNeeded :: a 
}

type Language = String

checkField :: Language -> GameField -> Maybe (TranslationNeeded a)
checkField lang itm = undefined

-- findNoField :: [[GameItem]] -> [Maybe [TranslationNeeded a]]
-- findNoField [eng:gs] = map (description . fields) gs
-- findNoField [eng:gs] = map filterField gs
--   where filterField [others] = 