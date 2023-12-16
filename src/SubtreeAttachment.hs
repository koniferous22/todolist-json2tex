module SubtreeAttachment
  ( LeafNodeAttachmentBehaviour(..)
  , attachSubtrees
  ) where

import Control.Monad (foldM)
import SubtreeAttachment.Data (SubtreeAttachment(..))
import SubtreeAttachment.LeafNodeAttachmentBehaviour
  ( LeafNodeAttachmentBehaviour(..)
  )
import TodoNode.Data (TodoNode(..))
import TodoSection.Data (TodoSection(..))

compareWithMaybe :: (Eq a) => a -> Maybe a -> Bool
compareWithMaybe a = (Just a ==)

attachSubtreesToListOfSections ::
     [TodoSection]
  -> LeafNodeAttachmentBehaviour
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
  -> LeafNodeAttachmentBehaviour
  -> SubtreeAttachment
  -> Either String TodoSection
attachSubtreesToSection (TodoSection title' alias' forest') b a =
  TodoSection title' alias' <$> attachSubtreesToListOfNodes forest' b a

attachSubtreesToListOfNodes ::
     [TodoNode]
  -> LeafNodeAttachmentBehaviour
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
  -> LeafNodeAttachmentBehaviour
  -> SubtreeAttachment
  -> Either String TodoNode
attachSubtreesToNode (TodoItemizeNode text' alias' conditions' children') b a =
  TodoItemizeNode text' alias' conditions' <$>
  attachSubtreesToListOfNodes children' b a
attachSubtreesToNode (TodoEnumerateNode text' alias' conditions' children') b a =
  TodoEnumerateNode text' alias' conditions' <$>
  attachSubtreesToListOfNodes children' b a
attachSubtreesToNode (TodoTextNode text' alias' conditions') b@ReplaceWithItemizeNode a =
  TodoItemizeNode text' alias' conditions' <$>
  attachSubtreesToListOfNodes [] b a
attachSubtreesToNode (TodoTextNode text' alias' conditions') b@ReplaceWithEnumerateNode a =
  TodoEnumerateNode text' alias' conditions' <$>
  attachSubtreesToListOfNodes [] b a
attachSubtreesToNode _ _ _ =
  Left "Cannot attach to leaf node - see CLI option --leaf-attachment-behaviour"

attachSubtrees ::
     LeafNodeAttachmentBehaviour
  -> [TodoSection]
  -> [SubtreeAttachment]
  -> Either String [TodoSection]
attachSubtrees b = foldM (\acc val -> attachSubtreesToListOfSections acc b val)
