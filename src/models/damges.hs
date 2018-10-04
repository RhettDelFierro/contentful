module Models.Games where

data Locale = Locale {
    code :: String
  , defaultLocale :: Bool
  , localeName :: String
  , fallbackCode :: String
} deriving (Show, Eq)

