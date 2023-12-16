{-# LANGUAGE DeriveGeneric #-}

module TodoSection.Data
  ( TodoSection(..)
  ) where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import TodoNode.Data (TodoNode(..))

data TodoSection = TodoSection
  { sectionTitle :: String
  , sectionAlias :: String
  , forest :: [TodoNode]
  } deriving (Generic, Show)

instance FromJSON TodoSection
