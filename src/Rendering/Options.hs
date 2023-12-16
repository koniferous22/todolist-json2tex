module Rendering.Options
  ( TexRenderOptions(..)
  ) where

data TexRenderOptions = TexRenderOptions
  { enumerateTag :: String
  , itemizeTag :: String
  } deriving (Show)
