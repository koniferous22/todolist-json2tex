module SubtreeAttachment.LeafNodeAttachmentBehaviour
  ( LeafNodeAttachmentBehaviour(..)
  ) where

data LeafNodeAttachmentBehaviour
  = Fail
  | ReplaceWithEnumerateNode
  | ReplaceWithItemizeNode
  deriving (Show)
