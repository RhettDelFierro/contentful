module Lib ( getAllHardwareSpecificationIO
           , getAllGameIO
) where

import System.Environment (getEnv)
import Data.ByteString.UTF8 hiding (foldl)
import Models.GlobalModels
import Models.Game(GameItem)
import Models.HardwareSpecification(HardwareSpecificationItem)
import Queries.HardwareSpecificationQueries
import Queries.GameQueries


type EnvironmentValue = String

data EnvironmentConfig = EnvironmentConfig {
      preview_access_token_prod :: EnvironmentValue
    , space_contentful_launcher_prod :: EnvironmentValue
    , preview_access_token_sandbox :: EnvironmentValue
    , space_contentful_launcher_sandbox :: EnvironmentValue
} 

getEnvironmentVars :: IO EnvironmentConfig
getEnvironmentVars = do 
    token_prod <- getEnv "PREVIEW_ACCESS_TOKEN_PROD"
    space_prod <- getEnv "SPACE_CONTENTFUL_LAUNCHER_PROD"
    token_sandbox <- getEnv "PREVIEW_ACCESS_TOKEN"
    space_sandbox <- getEnv "SPACE_CONTENTFUL"
    return $ EnvironmentConfig token_prod space_prod token_sandbox space_sandbox

getAllGameIO :: IO [GameItem]
getAllGameIO = do
    config <- getEnvironmentVars
    gs <- getGameAPI $ buildQueryGame $ fromString $ preview_access_token_sandbox config
    -- undefined
    return $ items gs

getAllHardwareSpecificationIO :: IO [HardwareSpecificationItem]
getAllHardwareSpecificationIO = do
    config <- getEnvironmentVars
    hws <- getHardwareSpecificationAPI $ buildQueryHardwareSpecification $ fromString $ preview_access_token_sandbox config
    return $ items hws