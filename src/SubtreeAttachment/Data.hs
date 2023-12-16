{-# LANGUAGE DeriveGeneric #-}

module SubtreeAttachment.Data
  ( SubtreeAttachment(..)
  ) where

import Data.Aeson (FromJSON)
import GHC.Generics
import TodoNode.Data (TodoNode(..))

data SubtreeAttachment = SubtreeAttachment
  { attachTo :: [String]
  , subtrees :: [TodoNode]
  } deriving (Show, Generic)

instance FromJSON SubtreeAttachment
