{-# LANGUAGE OverloadedStrings #-}

import ContentApiModel (ApiConfig, TagSearchQuery, TagSearchResult)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Conduit


makeUrl :: ApiConfig -> TagSearchQuery -> String
makeUrl config query = T.unpack $ T.concat [endpoint config, "/tags?q=", q query]

tagSearch :: ApiConfig -> TagSearchQuery -> IO TagSearchResult
tagSearch config query = runResourceT $ do
  req <- parseUrl (makeUrl config query)
  man <- liftIO $ newManager conduitManagerSettings
  response <- httpLbs req man
  let tagResult = decode $ responseBody response in
      case tagResult of
        Just result -> return result
        Nothing -> liftIO $ mzero

main :: IO ()
main = do
  response <- tagSearch (ApiConfig "http://content.guardianapis.com" Nothing) (TagSearchQuery "video")
  putStrLn "Found tags:"
  mapM_ putStrLn (map (T.unpack . unTagId . tagId) (results response))
