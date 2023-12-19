module TodoNode.Resolve
  ( resolveTodoNodes
  ) where

import Conditions.Resolve (resolveCondition)
import Ctx (Ctx)
import Data.Maybe (mapMaybe)
import TodoNode.Data (TodoNode(..))

resolveTodoNode :: Ctx -> TodoNode -> Maybe TodoNode
resolveTodoNode ctx n@(TodoTextNode _ conditions' _) =
  if maybe True (resolveCondition ctx) conditions'
    then Just n
    else Nothing
resolveTodoNode ctx (TodoItemizeNode alias' conditions' text' children') =
  if maybe True (resolveCondition ctx) conditions'
    then Just
           (TodoItemizeNode
              alias'
              conditions'
              text'
              (resolveTodoNodes ctx children'))
    else Nothing
resolveTodoNode ctx (TodoEnumerateNode alias' conditions' text' children') =
  if maybe True (resolveCondition ctx) conditions'
    then Just
           (TodoEnumerateNode
              alias'
              conditions'
              text'
              (resolveTodoNodes ctx children'))
    else Nothing
resolveTodoNode ctx (TodoFragmentNode alias' conditions' children') =
  if maybe True (resolveCondition ctx) conditions'
    then Just
           (TodoFragmentNode
              alias'
              conditions'
              (resolveTodoNodes ctx children'))
    else Nothing

resolveTodoNodes :: Ctx -> [TodoNode] -> [TodoNode]
resolveTodoNodes d = mapMaybe (resolveTodoNode d)
