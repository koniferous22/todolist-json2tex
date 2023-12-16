module TodoSection.Resolve
  ( resolveTodoSections
  ) where

import Ctx (Ctx)
import TodoNode.Resolve (resolveTodoNodes)
import TodoSection.Data (TodoSection(..))

resolveTodoSection :: Ctx -> TodoSection -> TodoSection
resolveTodoSection ctx (TodoSection t a f) =
  TodoSection t a . resolveTodoNodes ctx $ f

resolveTodoSections :: Ctx -> [TodoSection] -> [TodoSection]
resolveTodoSections ctx = map (resolveTodoSection ctx)
