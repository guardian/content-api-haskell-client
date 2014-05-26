{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Network.Guardian.ContentApi
import Network.Guardian.ContentApi.Content
import Network.Guardian.ContentApi.URL
import Network (withSocketsDo)
import Options.Applicative

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

parseQuery :: Parser ContentSearchQuery
parseQuery = ContentSearchQuery
  <$> optional (Text.pack <$> strOption (short 'q' <> help "query text"))
  <*> many (Text.pack <$> strOption (long "section" <> help "sections"))
  <*> pure []

summarise :: Content -> Text.Text
summarise content =
  Text.unlines [ webTitle content, unURL (webUrl content) ]

main :: IO ()
main = withSocketsDo $ do
  query    <- execParser $ info (helper <*> parseQuery) fullDesc
  config   <- defaultApiConfig Nothing
  response <- runContentApi config $ contentSearch query
  forM_ (results response) (Text.putStrLn . summarise)

