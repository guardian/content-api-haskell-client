{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad

import Network.Guardian.ContentApi
import Network.Guardian.ContentApi.Tag
import Network (withSocketsDo)

import qualified Data.Text.IO as TIO

main :: IO ()
main = withSocketsDo $ do
  config   <- defaultApiConfig Nothing
  response <- runContentApi config $ tagSearch (TagSearchQuery "video")
  putStrLn "Found tags:"
  forM_ (results response) $ TIO.putStrLn . unTagId . tagId
