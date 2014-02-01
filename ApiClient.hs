{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import Control.Exception      (Exception, catch, throwIO)
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Data.Aeson
import Data.Conduit
import Data.Typeable          (Typeable)
import Data.Text              (Text)
import Network                (withSocketsDo)
import Network.HTTP.Conduit
import Network.HTTP.Types

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

newtype URL = URL { unURL :: Text } deriving (Show)
newtype TagId = TagId { unTagId :: Text } deriving (Show)
newtype ReferenceType = ReferenceType { unReferenceType :: Text } deriving (Show)

data Reference = Reference {
    referenceType :: ReferenceType
  , referenceId :: Text
  } deriving (Show)

-- Although it's not represented like this either in the JSON or the Scala 
-- client, given these fields will both either exist or not exist, I think it 
-- makes sense for there to be a single type wrapping both
data Section = Section {
    sectionId :: Text
  , name :: Text
  } deriving (Show)

-- Currently just copying the Scala client's implementation. It would certainly 
-- be nicer to clean this up a lot. Byline images, for example, are only really 
-- relevant for contributors. We could possibly do away with 'tagType' here and 
-- have proper disjoint types.
data Tag = Tag {
    tagId :: TagId
  , tagType :: Text
  , section :: Maybe Section
  , webTitle :: Text
  , webUrl :: URL 
  , apiUrl :: URL 
  , references :: Maybe [Reference] 
  , bio :: Maybe Text
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
    q :: Text
  } deriving (Show)

data TagSearchResult = TagSearchResult {
    status :: Text
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

data ContentApiError = InvalidApiKey
                     | OtherContentApiError Int Text
                       deriving (Typeable, Show, Eq)

instance Exception ContentApiError

data ApiConfig = ApiConfig {
    endpoint :: Text
  , apiKey :: Maybe Text
  } deriving (Show)

makeUrl :: ApiConfig -> TagSearchQuery -> String
makeUrl (ApiConfig endpoint key) query = T.unpack $
  T.concat [endpoint, "/tags?q=", q query, maybe "" (T.append "&api-key=") key]

tagSearch :: ApiConfig -> TagSearchQuery -> IO TagSearchResult
tagSearch config query = runResourceT $ do
  req <- parseUrl (makeUrl config query)
  man <- liftIO $ newManager conduitManagerSettings
  response <- liftIO $ catch (httpLbs req man)
    (\e -> case e :: HttpException of
      StatusCodeException _ headers _ ->
        maybe (throwIO e) throwIO (contentApiError headers)
      _ -> throwIO e)
  let tagResult = decode $ responseBody response
  case tagResult of
    Just result -> return result
    Nothing -> liftIO $ mzero

contentApiError :: ResponseHeaders -> Maybe ContentApiError
contentApiError headers = case lookup "X-Mashery-Error-Code" headers of
  Just "ERR_403_DEVELOPER_INACTIVE" -> Just InvalidApiKey
  _ -> Nothing

main :: IO ()
main = withSocketsDo $ do
  response <- tagSearch (ApiConfig "http://content.guardianapis.com" Nothing) (TagSearchQuery "video")
  putStrLn "Found tags:"
  mapM_ (TIO.putStrLn . unTagId . tagId) (results response)
