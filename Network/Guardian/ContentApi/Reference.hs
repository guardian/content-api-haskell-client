{-# LANGUAGE OverloadedStrings #-}

module Network.Guardian.ContentApi.Reference where

import Control.Monad (mzero)
import Data.Aeson
import Data.Text     (Text)

newtype ReferenceType = ReferenceType { unReferenceType :: Text } deriving (Show)

data Reference = Reference {
    referenceType :: ReferenceType
  , referenceId :: Text
  } deriving Show

instance FromJSON Reference where
  parseJSON (Object v) = do
    referenceId <- v .: "id"
    referenceType <- v .: "type"
    return $ Reference (ReferenceType referenceType) referenceId
    
  parseJSON _ = mzero
