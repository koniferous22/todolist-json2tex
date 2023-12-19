{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module TodoNode.Data
  ( TodoNode(..)
  ) where

import Conditions.Data (Condition(..))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe(fromMaybe)
import GHC.Generics

data TodoNode
  = TodoTextNode
      {
        alias :: Maybe String
      , conditions :: Maybe Condition
      , text :: String
      }
  | TodoItemizeNode
      { 
        alias :: Maybe String
      , conditions :: Maybe Condition
      , text :: String
      , children :: [TodoNode]
      }
  | TodoEnumerateNode
      { alias :: Maybe String
      , conditions :: Maybe Condition
      , text :: String
      , children :: [TodoNode]
      }
  | TodoFragmentNode
    {
        alias :: Maybe String
      , conditions :: Maybe Condition
      , children :: [TodoNode]
    }
  deriving (Show, Generic)

instance FromJSON TodoNode where
  parseJSON =
    withObject "TodoNode" $ \obj -> do
      tag <- obj .: "type" :: Parser String
      alias' <- obj .:? "alias"
      conditions' <- obj .:? "conditions"
      case tag of
        "textNode" -> TodoTextNode alias' conditions' <$> obj .: "text"
        "ulNode" ->
          TodoItemizeNode alias' conditions' <$> obj .: "text" <*> (fromMaybe [] <$> obj .:? "children")
        "olNode" ->
          TodoEnumerateNode alias' conditions' <$> obj .: "text" <*> (fromMaybe [] <$> obj .:? "children")
        "fragmentNode" ->
          TodoFragmentNode alias' conditions' <$> (fromMaybe [] <$> obj .:? "children")
        _ -> fail "Invalid tag"
