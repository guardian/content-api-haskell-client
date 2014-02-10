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
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.Aeson      (FromJSON, decode)
import Data.Conduit
import Data.Maybe      (catMaybes, maybeToList)
import Data.Monoid
import Data.Typeable   (Typeable)
import Data.Text       (Text)

import Network.HTTP.Conduit
import Network.HTTP.Types

import qualified Data.ByteString.Char8 as BC

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
contentSearch = search contentSearchUrl

tagSearch :: TagSearchQuery -> ContentApi TagSearchResult
tagSearch = search tagSearchUrl

search :: (FromJSON r) => (a -> ContentApi String) -> a -> ContentApi r
search reqToUrl req = do
  ApiConfig _ _ mgr <- ask
  url      <- reqToUrl req
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
  mkUrl ["search"] $ catMaybes [
      mkParam "q"       csQueryText
    , mkParam "section" csSection
    ]

tagSearchUrl :: TagSearchQuery -> ContentApi String
tagSearchUrl TagSearchQuery {..} =
  mkUrl ["tags"] $ catMaybes [
      mkParam "q"       tsQueryText
    , mkParam "section" tsSection
    , mkParam "type"    tsTagType
    ]

mkUrl :: [Text] -> QueryText -> ContentApi String
mkUrl path query = do
  ApiConfig endpoint key _ <- ask
  let query' = queryTextToQuery $ maybeToList (mkParam "api-key" key) ++ query
  return $ BC.unpack . toByteString $ endpoint <> encodePath path query'

mkParam :: Text -> Maybe Text -> Maybe (Text, Maybe Text)
mkParam k = fmap $ \v -> (k, Just v) 

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
