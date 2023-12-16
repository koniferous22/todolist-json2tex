module Conditions.Resolve
  ( resolveCondition
  ) where

import Conditions.Data (AllowedDays(..), Condition(..))
import Ctx (Ctx, resolveConditionalEnvVariable, resolveDayOfWeek)
import qualified Data.Map.Strict as Map

resolveCondition :: Ctx -> Condition -> Bool
resolveCondition ctx (DayOfWeekCondition allowedDays') =
  Map.findWithDefault False (resolveDayOfWeek ctx) (allowedDaysMap allowedDays')
resolveCondition ctx (EnvVariableCondition envVariable') =
  resolveConditionalEnvVariable ctx envVariable'
resolveCondition ctx (AndOperator conditions') =
  all (resolveCondition ctx) conditions'
resolveCondition ctx (OrOperator conditions') =
  any (resolveCondition ctx) conditions'
resolveCondition ctx (NotOperator condition) =
  not (resolveCondition ctx condition)
