{-# LANGUAGE OverloadedStrings #-}

module Models.PatchNote where

import Models.GlobalModels (SysItem, SysLink)
import Control.Monad
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format

data AllPatchNoteQuery = AllPatchNoteQuery {
  topSys :: SysLink
  , total    :: Integer
  , skip     :: Integer
  , limit    :: Integer
  , items    :: [PatchNoteItem]
} deriving (Show, Eq)

instance FromJSON AllPatchNoteQuery where
    parseJSON (Object o) =
      AllPatchNoteQuery <$> (o .: "sys")
                        <*> (o .: "total")
                        <*> (o .: "skip")
                        <*> (o .: "limit")
                        <*> (o .: "items")
    parseJSON _          = mzero

data PatchNoteItem = PatchNoteItem {
  sys :: SysItem
  , fields :: PatchNoteField
} deriving (Show, Eq)

instance FromJSON PatchNoteItem where
    parseJSON (Object o) = PatchNoteItem <$> (o .: "sys") <*> (o .: "fields")
    parseJSON _          = mzero

data PatchNoteField = PatchNoteField {
  title :: String
  , body :: String
  , version :: String
} deriving (Show,Eq)

instance FromJSON PatchNoteField where
    parseJSON (Object o) = 
        PatchNoteField <$> (o .: "title") 
                       <*> (o .: "body")
                       <*> (o .: "version")
    parseJSON _          = mzero