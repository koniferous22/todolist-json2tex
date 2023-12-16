module TodoNode.Resolve
  ( resolveTodoNodes
  ) where

import Conditions.Resolve (resolveCondition)
import Ctx (Ctx)
import Data.Maybe (mapMaybe)
import TodoNode.Data (TodoNode(..))

resolveTodoNode :: Ctx -> TodoNode -> Maybe TodoNode
resolveTodoNode ctx n@(TodoTextNode _ _ conditions') =
  if maybe True (resolveCondition ctx) conditions'
    then Just n
    else Nothing
resolveTodoNode ctx (TodoItemizeNode text' alias' conditions' children') =
  if maybe True (resolveCondition ctx) conditions'
    then Just
           (TodoItemizeNode
              text'
              alias'
              conditions'
              (resolveTodoNodes ctx children'))
    else Nothing
resolveTodoNode ctx (TodoEnumerateNode text' alias' conditions' children') =
  if maybe True (resolveCondition ctx) conditions'
    then Just
           (TodoEnumerateNode
              text'
              alias'
              conditions'
              (resolveTodoNodes ctx children'))
    else Nothing

resolveTodoNodes :: Ctx -> [TodoNode] -> [TodoNode]
resolveTodoNodes d = mapMaybe (resolveTodoNode d)
