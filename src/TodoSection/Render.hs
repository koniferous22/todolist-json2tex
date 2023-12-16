module TodoSection.Render
  ( renderTexSections
  ) where

import qualified Data.ByteString.Builder as BB
import Rendering.Options (TexRenderOptions(..))
import Rendering.Utils (repeatChar)
import TodoNode.Render (renderTexNodes)
import TodoSection.Data (TodoSection(..))

renderTexSections :: TexRenderOptions -> [TodoSection] -> BB.Builder
renderTexSections opts sections = mconcat (map (renderTexSection opts) sections)

renderTexSection :: TexRenderOptions -> TodoSection -> BB.Builder
renderTexSection opts (TodoSection t _ f) =
  BB.stringUtf8 "\\section{" <>
  BB.stringUtf8 t <>
  BB.stringUtf8 "}\n\\begin{" <>
  BB.stringUtf8 (itemizeTag opts) <>
  BB.stringUtf8 "}\n" <>
  renderTexNodes opts 2 f <>
  BB.stringUtf8 "\\end{" <>
  BB.stringUtf8 (itemizeTag opts) <> BB.stringUtf8 "}\n"
