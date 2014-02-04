{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.Guardian.ContentApi
  (
    ContentApi
  , runContentApi
  , ApiConfig
  , defaultApiConfig
  , ContentApiError
  , tagSearch
  ) where

import Network.Guardian.ContentApi.Tag

import Blaze.ByteString.Builder (Builder, fromByteString, toByteString)

import Control.Exception.Lifted
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.Aeson      (decode)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Foldable   (foldMap)
import Data.Monoid
import Data.Typeable   (Typeable)
import Data.Text       (Text)

import Network.HTTP.Conduit
import Network.HTTP.Types

import qualified Data.ByteString.Char8 as BC

type ContentApi a = ReaderT ApiConfig (ResourceT IO) a

runContentApi :: MonadIO f => ApiConfig -> ContentApi a -> f a
runContentApi config action = liftIO . runResourceT $ runReaderT action config

type ApiKey = ByteString

data ApiConfig = ApiConfig {
    endpoint :: Builder
  , apiKey :: Maybe ApiKey
  , manager :: Manager
  }

data ContentApiError = InvalidApiKey
                     | OtherContentApiError Int Text
                       deriving (Typeable, Show, Eq)

instance Exception ContentApiError

tagSearch :: TagSearchQuery -> ContentApi TagSearchResult
tagSearch query = do
  ApiConfig _ _ mgr <- ask
  url <- makeUrl query
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

makeUrl :: TagSearchQuery -> ContentApi String
makeUrl (TagSearchQuery q) = do
  ApiConfig endpoint key _ <- ask
  let query = ("q", Just q) : foldMap (\k -> [("api-key", Just k)]) key
  return $ BC.unpack . toByteString $ endpoint <> encodePath ["tags"] query

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
