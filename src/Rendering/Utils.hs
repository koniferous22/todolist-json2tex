module Rendering.Utils
  ( repeatChar
  ) where

import qualified Data.ByteString.Builder as BB

repeatChar :: Char -> Int -> BB.Builder
repeatChar c n = mconcat (replicate n (BB.charUtf8 c))
