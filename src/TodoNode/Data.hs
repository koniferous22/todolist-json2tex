{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module TodoNode.Data
  ( TodoNode(..)
  ) where

import Conditions.Data (Condition(..))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import GHC.Generics

data TodoNode
  = TodoTextNode
      { alias :: Maybe String
      , conditions :: Maybe Condition
      , text :: String
      }
  | TodoItemizeNode
      { alias :: Maybe String
      , conditions :: Maybe Condition
      , text :: String
      , skipIfNoChildren :: Bool
      , children :: [TodoNode]
      }
  | TodoEnumerateNode
      { alias :: Maybe String
      , conditions :: Maybe Condition
      , text :: String
      , skipIfNoChildren :: Bool
      , children :: [TodoNode]
      }
  deriving (Show, Generic)

instance FromJSON TodoNode where
  parseJSON =
    withObject "TodoNode" $ \obj -> do
      tag <- obj .: "type" :: Parser String
      alias' <- obj .:? "alias"
      conditions' <- obj .:? "conditions"
      text' <- obj .: "text"
      case tag of
        "textNode" -> return . TodoTextNode alias' conditions' $ text'
        "ulNode" ->
          TodoItemizeNode alias' conditions' text' <$>
          (fromMaybe False <$> obj .:? "skipIfNoChildren") <*>
          (fromMaybe [] <$> obj .:? "children")
        "olNode" ->
          TodoEnumerateNode alias' conditions' text' <$>
          (fromMaybe False <$> obj .:? "skipIfNoChildren") <*>
          (fromMaybe [] <$> obj .:? "children")
        _ -> fail "Invalid tag"
