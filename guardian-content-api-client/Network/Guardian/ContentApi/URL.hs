module Network.Guardian.ContentApi.URL where

import Data.Text (Text)

newtype URL = URL { unURL :: Text } deriving Show
