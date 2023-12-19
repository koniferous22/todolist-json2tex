module SubtreeAttachment.AttachmentBehaviour
  ( TextNodeAttachmentBehaviour(..)
  ) where

data TextNodeAttachmentBehaviour
  = Fail
  | ReplaceTextWithEnumerateNode
  | ReplaceTextWithItemizeNode
  deriving (Show)
