module Rendering.Tex
  ( renderTex
  ) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Rendering.Options (TexRenderOptions(..))
import Text.Mustache
import TodoSection.Data (TodoSection(..))
import TodoSection.Render (renderTexSections)

renderTex :: TexRenderOptions -> [TodoSection] -> BL.ByteString
renderTex opts sections = BB.toLazyByteString (renderTexSections opts sections)
