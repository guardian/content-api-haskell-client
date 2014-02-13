{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Guardian.ContentApi
  (
    ContentApi
  , runContentApi
  , ApiConfig(..)
  , defaultApiConfig
  , ContentApiError
  , contentSearch
  , tagSearch
  ) where

import Network.Guardian.ContentApi.Content
import Network.Guardian.ContentApi.Tag

import Blaze.ByteString.Builder (Builder, fromByteString, toByteString)

import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.Aeson      (FromJSON, decode)
import Data.Conduit
import Data.Maybe      (maybeToList)
import Data.Monoid
import Data.Typeable   (Typeable)
import Data.Text       (Text)

import Network.HTTP.Conduit
import Network.HTTP.Types

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

type ContentApi a = ReaderT ApiConfig (ResourceT IO) a

runContentApi :: MonadIO f => ApiConfig -> ContentApi a -> f a
runContentApi config action = liftIO . runResourceT $ runReaderT action config

type ApiKey = Text

data ApiConfig = ApiConfig {
    endpoint :: Builder
  , apiKey :: Maybe ApiKey
  , manager :: Manager
  }

data ContentApiError = InvalidApiKey
                     | ParseError
                     | OtherContentApiError Int Text
                       deriving (Typeable, Show, Eq)

instance Exception ContentApiError

contentSearch :: ContentSearchQuery -> ContentApi ContentSearchResult
contentSearch = search <=< contentSearchUrl

tagSearch :: TagSearchQuery -> ContentApi TagSearchResult
tagSearch = search <=< tagSearchUrl

search :: (FromJSON r) => String -> ContentApi r
search url = do
  ApiConfig _ _ mgr <- ask
  httpReq  <- parseUrl url
  response <- catch (httpLbs httpReq mgr)
                (\e -> case e :: HttpException of
                  StatusCodeException _ headers _ ->
                    maybe (throwIO e) throwIO (contentApiError headers)
                  _ -> throwIO e)
  let searchResult = decode $ responseBody response
  maybe (throwIO ParseError) return searchResult

contentSearchUrl :: ContentSearchQuery -> ContentApi String
contentSearchUrl ContentSearchQuery {..} =
  mkUrl ["search"] $ param "q" csQueryText
                  <> sectionParam csSection
                  <> fieldsParam csShowFields

tagSearchUrl :: TagSearchQuery -> ContentApi String
tagSearchUrl TagSearchQuery {..} =
  mkUrl ["tags"] $ param "q" tsQueryText
                <> sectionParam tsSection
                <> param "type" tsTagType

mkUrl :: [Text] -> QueryText -> ContentApi String
mkUrl path query = do
  ApiConfig endpoint key _ <- ask
  let query' = queryTextToQuery $ param "api-key" key <> query
  return $ BC.unpack . toByteString $ endpoint <> encodePath path query'

fieldsParam :: [Text] -> QueryText
fieldsParam = multiParam "fields" ","

sectionParam :: [Text] -> QueryText
sectionParam = multiParam "section" "|"

param :: Text -> Maybe Text -> QueryText
param k = maybeToList . fmap (\v -> (k, Just v))

multiParam :: Text -> Text -> [Text] -> QueryText
multiParam _ _   [] = []
multiParam k sep vs = [(k, Just $ T.intercalate sep vs)]

contentApiError :: ResponseHeaders -> Maybe ContentApiError
contentApiError headers = case lookup "X-Mashery-Error-Code" headers of
  Just "ERR_403_DEVELOPER_INACTIVE" -> Just InvalidApiKey
  _ -> Nothing

defaultApiConfig :: MonadIO f => Maybe ApiKey -> f ApiConfig
defaultApiConfig key = do
  man <- liftIO $ newManager conduitManagerSettings
  return $ ApiConfig defaultEndpoint key man
  where
    defaultEndpoint = fromByteString "http://content.guardianapis.com"
