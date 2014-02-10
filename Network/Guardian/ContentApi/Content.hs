{-# LANGUAGE OverloadedStrings #-}

module Network.Guardian.ContentApi.Content
 (
   Content(..)
 , ContentId(..)
 , ContentSearchQuery(..)
 , ContentSearchResult(..)
 ) where

import Network.Guardian.ContentApi.Reference
import Network.Guardian.ContentApi.Section
import Network.Guardian.ContentApi.Tag
import Network.Guardian.ContentApi.URL

import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Foldable (fold)
import Data.Map      (Map)
import Data.Text     (Text, unpack)
import Data.Thyme

import System.Locale (defaultTimeLocale)

data Content = Content {
    contentId :: ContentId
  , section :: Maybe Section
  , webPublicationDate :: UTCTime
  , webTitle :: Text
  , webUrl :: URL
  , apiUrl :: URL
  , fields :: Map Text Text
  , tags :: [Tag]
  -- , factboxes :: [Factbox]
  -- , elements :: [Element]
  -- , snippets :: Map Text Text
  , references :: [Reference]
  } deriving Show

newtype ContentId = ContentId { unContentId :: Text } deriving Show

data ContentSearchQuery = ContentSearchQuery {
    csQueryText  :: Maybe Text
  , csSection    :: Maybe Text
  , csShowFields :: [Text]
  } deriving Show

data ContentSearchResult = ContentSearchResult {
    status :: Text
  , totalResults :: Int
  , startIndex :: Int
  , pageSize :: Int
  , currentPage :: Int
  , pages :: Int
  , results :: [Content]
  } deriving Show

instance FromJSON Content where
  parseJSON (Object v) = do
    id          <- v .:  "id"
    sectionId   <- v .:? "sectionId"
    sectionName <- v .:? "sectionName"
    webPubDate  <- v .:  "webPublicationDate"
    webTitle    <- v .:  "webTitle"
    webUrl      <- v .:  "webUrl"
    apiUrl      <- v .:  "apiUrl"
    tags        <- v .:? "tags"
    fields      <- v .:? "fields"
    references  <- v .:? "references"
    Just wpd    <- return $ parseUTCTime webPubDate
    return $
      Content (ContentId id)
              (Section <$> sectionId <*> sectionName)
              wpd
              webTitle
              (URL webUrl)
              (URL apiUrl)
              (fold fields)
              (fold tags)
              (fold references)
  parseJSON _ = mzero

instance FromJSON ContentSearchResult where
  parseJSON (Object v) = do
    r            <- v .: "response"
    status       <- r .: "status"
    totalResults <- r .: "total"
    startIndex   <- r .: "startIndex"
    pageSize     <- r .: "pageSize"
    currentPage  <- r .: "currentPage"
    pages        <- r .: "pages"
    results      <- r .: "results"
    return $
      ContentSearchResult status
                          totalResults
                          startIndex
                          pageSize
                          currentPage
                          pages
                          results
  parseJSON _ = mzero

parseUTCTime :: Text -> Maybe UTCTime
parseUTCTime = parseTime defaultTimeLocale "%FT%H:%M:%S%Q%Z" . unpack
