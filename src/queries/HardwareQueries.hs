{-# LANGUAGE OverloadedStrings #-}
module Queries.HardwareQueries where

import Network.HTTP.Simple
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

getEnvironmentVars :: IO EnvironmentConfig
getEnvironmentVars = do 
    token_prod <- getEnv "PREVIEW_ACCESS_TOKEN_PROD"
    space_prod <- getEnv "SPACE_CONTENTFUL_LAUNCHER_PROD"
    token_sandbox <- getEnv "PREVIEW_ACCESS_TOKEN"
    space_sandbox <- getEnv "SPACE_CONTENTFUL_LAUNCHER_SANDBOX"
    return $ EnvironmentConfig token_prod space_prod token_sandbox space_sandbox

getHardwareAPI :: IO AllHardwareQuery
getHardwareAPI = do
    config <- getEnvironmentVars