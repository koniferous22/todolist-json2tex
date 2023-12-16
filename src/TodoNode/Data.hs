{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module TodoNode.Data
  ( TodoNode(..)
  ) where

import Conditions.Data (Condition(..))
import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Generics

data TodoNode
  = TodoTextNode
      { text :: String
      , alias :: Maybe String
      , conditions :: Maybe Condition
      }
  | TodoItemizeNode
      { text :: String
      , alias :: Maybe String
      , conditions :: Maybe Condition
      , children :: [TodoNode]
      }
  | TodoEnumerateNode
      { text :: String
      , alias :: Maybe String
      , conditions :: Maybe Condition
      , children :: [TodoNode]
      }
  deriving (Show, Generic)

instance FromJSON TodoNode where
  parseJSON =
    withObject "TodoNode" $ \obj -> do
      tag <- obj .: "type" :: Parser String
      text' <- obj .: "text"
      alias' <- obj .:? "alias"
      conditions' <- obj .:? "conditions"
      case tag of
        "textNode" -> return $ TodoTextNode text' alias' conditions'
        "ulNode" ->
          TodoItemizeNode text' alias' conditions' <$> obj .: "children"
        "olNode" ->
          TodoEnumerateNode text' alias' conditions' <$> obj .: "children"
        _ -> fail "Invalid tag"
