module Network.Guardian.ContentApi.Section where

import Data.Text (Text)

data Section = Section {
    sectionId :: Text
  , name :: Text
  } deriving (Show)
