{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Guardian.ContentApi
  (
    ContentApi
  , runContentApi
  , ApiConfig
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

import Data.Aeson      (decode)
import Data.Conduit
import Data.Maybe      (catMaybes)
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
                     | OtherContentApiError Int Text
                       deriving (Typeable, Show, Eq)

instance Exception ContentApiError

contentSearch :: ContentSearchQuery -> ContentApi ContentSearchResult
contentSearch query = do
  ApiConfig _ _ mgr <- ask
  url <- mkContentSearchUrl query
  req <- parseUrl url
  response <- catch (httpLbs req mgr)
    (\e -> case e :: HttpException of
      StatusCodeException _ headers _ ->
        maybe (throwIO e) throwIO (contentApiError headers)
      _ -> throwIO e)
  let result = decode $ responseBody response
  case result of
    Just result -> return result
    Nothing -> throwIO $ OtherContentApiError (-1) "Parse Error"
    
mkContentSearchUrl :: ContentSearchQuery -> ContentApi String
mkContentSearchUrl ContentSearchQuery {..} = do
  ApiConfig endpoint key _ <- ask
  let query = queryTextToQuery $ catMaybes [
                  mkParam "api-key" key
                , mkParam "q"       csQueryText
                , mkParam "section" csSection
                ]
  return $ BC.unpack . toByteString $ endpoint <> encodePath ["search"] query

tagSearch :: TagSearchQuery -> ContentApi TagSearchResult
tagSearch query = do
  ApiConfig _ _ mgr <- ask
  url <- mkTagSearchUrl query
  req <- parseUrl url
  response <- catch (httpLbs req mgr)
    (\e -> case e :: HttpException of
      StatusCodeException _ headers _ ->
        maybe (throwIO e) throwIO (contentApiError headers)
      _ -> throwIO e)
  let tagResult = decode $ responseBody response
  case tagResult of
    Just result -> return result
    Nothing -> throwIO $ OtherContentApiError (-1) "Parse Error"

mkTagSearchUrl :: TagSearchQuery -> ContentApi String
mkTagSearchUrl TagSearchQuery {..} = do
  ApiConfig endpoint key _ <- ask
  let query = queryTextToQuery $ catMaybes [
                  mkParam "api-key" key
                , mkParam "q"       tsQueryText
                , mkParam "section" tsSection
                , mkParam "type"    tsTagType
                ]
  return $ BC.unpack . toByteString $ endpoint <> encodePath ["tags"] query

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
