{-# LANGUAGE OverloadedStrings #-}

module Rendering.Mustache
  ( MustacheCtx(..)
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Rendering.Tex (renderTex)
import Text.Mustache (Template(..), ToMustache(..), (~>), object, substitute)

-- renderMustacheTemplate :: Template -> String -> String -> [TodoSection] -> BL.ByteString
-- renderMustacheTemplate templ title' subtitle' todo =
data MustacheCtx = MustacheCtx
  { title :: String
  , subtitle :: String
  , todoContents :: BL.ByteString
  }

instance ToMustache MustacheCtx where
  toMustache ctx =
    object
      [ "title" ~> title ctx
      , "subtitle" ~> subtitle ctx
      , "todoContents" ~> (TE.decodeUtf8 . BL.toStrict . todoContents) ctx
      ]
