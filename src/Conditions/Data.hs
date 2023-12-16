{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Conditions.Data
  ( Condition(..)
  , AllowedDays(..)
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Map.Strict as Map
import Data.Time (DayOfWeek(..))
import GHC.Generics

newtype AllowedDays = AllowedDays
  { allowedDaysMap :: Map.Map DayOfWeek Bool
  } deriving (Show, Generic)

data Condition
  = DayOfWeekCondition
      { allowedDays :: AllowedDays
      }
  | EnvVariableCondition
      { envVariable :: String
      }
  | AndOperator
      { conditions :: [Condition]
      }
  | OrOperator
      { conditions :: [Condition]
      }
  | NotOperator
      { negatedCondition :: Condition
      }
  deriving (Show, Generic)

instance FromJSON AllowedDays where
  parseJSON =
    withObject "AllowedDays" $ \v -> do
      key1Val <- v .:? "Monday" .!= False
      key2Val <- v .:? "Tuesday" .!= False
      key3Val <- v .:? "Wednesday" .!= False
      key4Val <- v .:? "Thursday" .!= False
      key5Val <- v .:? "Friday" .!= False
      key6Val <- v .:? "Saturday" .!= False
      key7Val <- v .:? "Sunday" .!= False
      let resultMap =
            Map.fromList
              [ (Monday, key1Val)
              , (Tuesday, key2Val)
              , (Wednesday, key3Val)
              , (Thursday, key4Val)
              , (Friday, key5Val)
              , (Saturday, key6Val)
              , (Sunday, key7Val)
              ]
      return . AllowedDays $ resultMap

instance FromJSON Condition where
  parseJSON =
    withObject "Condition" $ \obj -> do
      tag <- obj .: "type" :: Parser String
      case tag of
        "dayOfWeek" -> DayOfWeekCondition <$> obj .: "allowedDays"
        "envVariable" -> EnvVariableCondition <$> obj .: "envVariable"
        "andOperator" -> AndOperator <$> obj .: "conditions"
        "orOperator" -> OrOperator <$> obj .: "conditions"
        "notOperator" -> NotOperator <$> obj .: "condition"
        _ -> fail "Invalid tag"
