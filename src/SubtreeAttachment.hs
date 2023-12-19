module SubtreeAttachment
  ( TextNodeAttachmentBehaviour(..)
  , attachSubtrees
  ) where

import Control.Monad (foldM)
import SubtreeAttachment.Data (SubtreeAttachment(..))
import SubtreeAttachment.AttachmentBehaviour
  ( TextNodeAttachmentBehaviour(..)
  )
import TodoNode.Data (TodoNode(..))
import TodoSection.Data (TodoSection(..))

compareWithMaybe :: (Eq a) => a -> Maybe a -> Bool
compareWithMaybe a = (Just a ==)

attachSubtreesToListOfSections ::
     [TodoSection]
  -> TextNodeAttachmentBehaviour
  -> SubtreeAttachment
  -> Either String [TodoSection]
attachSubtreesToListOfSections (x:xs) b a@(SubtreeAttachment (y:ys) st) =
  if sectionAlias x == y
    then (: xs) <$> attachSubtreesToSection x b (SubtreeAttachment ys st)
    else (x :) <$> attachSubtreesToListOfSections xs b a
attachSubtreesToListOfSections (_:_) _ (SubtreeAttachment _ _) =
  Left "No attachment path specified - cannot attach on section level"
attachSubtreesToListOfSections _ _ (SubtreeAttachment (y:_) _) =
  Left ("Path \"" ++ y ++ "\" not found")
attachSubtreesToListOfSections _ _ (SubtreeAttachment _ _) = Left "Welp..."

attachSubtreesToSection ::
     TodoSection
  -> TextNodeAttachmentBehaviour
  -> SubtreeAttachment
  -> Either String TodoSection
attachSubtreesToSection (TodoSection title' alias' forest') b a =
  TodoSection title' alias' <$> attachSubtreesToListOfNodes forest' b a

attachSubtreesToListOfNodes ::
     [TodoNode]
  -> TextNodeAttachmentBehaviour
  -> SubtreeAttachment
  -> Either String [TodoNode]
attachSubtreesToListOfNodes (x:xs) b a@(SubtreeAttachment (y:ys) st) =
  if compareWithMaybe y (alias x)
    then (: xs) <$> attachSubtreesToNode x b (SubtreeAttachment ys st)
    else (x :) <$> attachSubtreesToListOfNodes xs b a
attachSubtreesToListOfNodes (x:xs) _ (SubtreeAttachment _ st) =
  Right (st ++ x : xs)
attachSubtreesToListOfNodes _ _ (SubtreeAttachment (y:_) _) =
  Left ("Path \"" ++ y ++ "\" not found")
attachSubtreesToListOfNodes _ _ (SubtreeAttachment _ st) = Right st

attachSubtreesToNode ::
     TodoNode
  -> TextNodeAttachmentBehaviour
  -> SubtreeAttachment
  -> Either String TodoNode
attachSubtreesToNode (TodoItemizeNode alias' conditions' text' children') b a =
  TodoItemizeNode alias' conditions' text' <$>
  attachSubtreesToListOfNodes children' b a
attachSubtreesToNode (TodoEnumerateNode alias' conditions' text' children') b a =
  TodoEnumerateNode alias' conditions' text' <$>
  attachSubtreesToListOfNodes children' b a
attachSubtreesToNode (TodoFragmentNode alias' conditions' children') b a =
  TodoFragmentNode alias' conditions' <$>
  attachSubtreesToListOfNodes children' b a
attachSubtreesToNode (TodoTextNode alias' conditions' text') b@ReplaceTextWithItemizeNode a =
  TodoItemizeNode alias' conditions' text' <$>
  attachSubtreesToListOfNodes [] b a
attachSubtreesToNode (TodoTextNode alias' conditions' text') b@ReplaceTextWithEnumerateNode a =
  TodoEnumerateNode alias' conditions' text' <$>
  attachSubtreesToListOfNodes [] b a
attachSubtreesToNode _ Fail _ =
  Left "Cannot attach to leaf node - see CLI option --leaf-attachment-behaviour"

attachSubtrees ::
     TextNodeAttachmentBehaviour
  -> [TodoSection]
  -> [SubtreeAttachment]
  -> Either String [TodoSection]
attachSubtrees b = foldM (\acc val -> attachSubtreesToListOfSections acc b val)
