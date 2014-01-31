{-# LANGUAGE OverloadedStrings #-}

module ContentApiModel where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.Text as T

newtype URL = URL { unURL :: T.Text } deriving (Show)
newtype TagId = TagId { unTagId :: T.Text } deriving (Show)
newtype ReferenceType = ReferenceType { unReferenceType :: T.Text } deriving (Show)

data Reference = Reference {
    referenceType :: ReferenceType
  , referenceId :: T.Text
  } deriving (Show)

-- Although it's not represented like this either in the JSON or the Scala
-- client, given these fields will both either exist or not exist, I think it
-- makes sense for there to be a single type wrapping both
data Section = Section {
    sectionId :: T.Text
  , name :: T.Text
  } deriving (Show)

-- Currently just copying the Scala client's implementation. It would certainly
-- be nicer to clean this up a lot. Byline images, for example, are only really
-- relevant for contributors. We could possibly do away with 'tagType' here and
-- have proper disjoint types.
data Tag = Tag {
    tagId :: TagId
  , tagType :: T.Text
  , section :: Maybe Section
  , webTitle :: T.Text
  , webUrl :: URL
  , apiUrl :: URL
  , references :: Maybe [Reference]
  , bio :: Maybe T.Text
  , bylineImageUrl :: Maybe URL
  , largeBylineImageUrl :: Maybe URL
  } deriving (Show)

instance FromJSON Reference where
  parseJSON (Object v) = do
    referenceId <- v .: "id"
    referenceType <- v .: "type"
    return $ Reference (ReferenceType referenceType) referenceId

  parseJSON _ = mzero

instance FromJSON Tag where
  parseJSON (Object v) = do
    tagId <- v .: "id"
    tagType <- v .: "type"
    sectionId <- v .:? "sectionId"
    sectionName <- v .:? "sectionName"
    webTitle <- v .: "webTitle"
    webUrl <- v .: "webUrl"
    apiUrl <- v .: "apiUrl"
    references <- v .:? "references"
    bio <- v .:? "bio"
    bylineImageUrl <- v .:? "bylineImageUrl"
    largeBylineImageUrl <- v .:? "bylineLargeImageUrl"
    return $ Tag (TagId tagId) tagType (Section <$> sectionId <*> sectionName)
      webTitle (URL webUrl) (URL apiUrl) references bio (URL <$> bylineImageUrl)
      (URL <$> largeBylineImageUrl)

  parseJSON _ = mzero

-- for now I'm just adding the search param
-- TODO: add all fields here http://explorer.content.guardianapis.com/#/tags?q=video
data TagSearchQuery = TagSearchQuery {
    q :: T.Text
  } deriving (Show)

data TagSearchResult = TagSearchResult {
    status :: T.Text
  , totalResults :: Int
  , startIndex :: Int
  , pageSize :: Int
  , currentPage :: Int
  , pages :: Int
  , results :: [Tag]
  } deriving (Show)

instance FromJSON TagSearchResult where
  parseJSON (Object v) = do
    r <- v .: "response"
    status <- r .: "status"
    totalResults <- r .: "total"
    startIndex <- r .: "startIndex"
    pageSize <- r .: "pageSize"
    currentPage <- r .: "currentPage"
    pages <- r .: "pages"
    results <- r .: "results"
    return $ TagSearchResult status totalResults startIndex pageSize
      currentPage pages results

  parseJSON _ = mzero

data ApiConfig = ApiConfig {
    endpoint :: T.Text
  , apiKey :: Maybe T.Text
  } deriving (Show)