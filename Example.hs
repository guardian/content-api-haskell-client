{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad

import Network.Guardian.ContentApi
import Network.Guardian.ContentApi.Content
import Network (withSocketsDo)

import qualified Data.Text.IO as TIO

main :: IO ()
main = withSocketsDo $ do
  config   <- defaultApiConfig Nothing
  response <- runContentApi config . contentSearch $
                ContentSearchQuery (Just "morrissey") [] []
  putStrLn "Found content:"
  forM_ (results response) $ TIO.putStrLn . unContentId . contentId
