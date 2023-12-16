module TodoNode.Render
  ( renderTexNodes
  ) where

import qualified Data.ByteString.Builder as BB
import Data.Monoid (mempty)
import Rendering.Options (TexRenderOptions(..))
import Rendering.Utils (repeatChar)
import TodoNode.Data (TodoNode(..))

renderTexNodes :: TexRenderOptions -> Int -> [TodoNode] -> BB.Builder
renderTexNodes opts offset nodes =
  mconcat (map (renderTexNode opts offset) nodes)

renderBeginEndForNonEmptyLists ::
     TexRenderOptions -> String -> Int -> [TodoNode] -> BB.Builder
renderBeginEndForNonEmptyLists opts _ _ [] = mempty
renderBeginEndForNonEmptyLists opts tag offset l@(_:_) =
  repeatChar ' ' offset <>
  BB.stringUtf8 "\\begin{" <>
  BB.stringUtf8 tag <>
  BB.stringUtf8 "}\n" <>
  renderTexNodes opts (offset + 2) l <>
  repeatChar ' ' offset <>
  BB.stringUtf8 "\\end{" <> BB.stringUtf8 tag <> BB.stringUtf8 "}\n"

renderTexNode :: TexRenderOptions -> Int -> TodoNode -> BB.Builder
renderTexNode _ offset (TodoTextNode t _ _) =
  repeatChar ' ' offset <>
  BB.stringUtf8 "\\item " <> BB.stringUtf8 t <> BB.charUtf8 '\n'
renderTexNode opts offset (TodoItemizeNode t _ _ children') =
  repeatChar ' ' offset <>
  BB.stringUtf8 "\\item " <>
  BB.stringUtf8 t <>
  BB.charUtf8 '\n' <>
  renderBeginEndForNonEmptyLists opts (itemizeTag opts) offset children'
renderTexNode opts offset (TodoEnumerateNode t _ _ children') =
  repeatChar ' ' offset <>
  BB.stringUtf8 "\\item " <>
  BB.stringUtf8 t <>
  BB.charUtf8 '\n' <>
  renderBeginEndForNonEmptyLists opts (enumerateTag opts) offset children'
