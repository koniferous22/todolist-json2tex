module Main
  ( main
  ) where

import Control.Monad (forM)
import Control.Monad.Except (liftIO, runExceptT)
import Ctx
  ( Ctx(..)
  , getCtx
  , resolveEnumerateTag
  , resolveItemizeTag
  , resolveMustacheTemplate
  , resolveSubtreeAttachmentBehaviour
  )
import Ctx.CliArgs (Todo2TexCliArgs(..))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import ErrorHandling (RuntimeMonad, concatResults, retrieveRight)
import Rendering.Mustache (MustacheCtx(MustacheCtx))
import Rendering.Options (TexRenderOptions(..))
import Rendering.Tex (renderTex)
import SubtreeAttachment (attachSubtrees)
import Text.Mustache (compileTemplate, substitute)
import TodoSection.Data (TodoSection(..))
import TodoSection.Resolve (resolveTodoSections)

readInput :: Maybe FilePath -> IO BL.ByteString
readInput Nothing = BL.getContents
readInput (Just filePath) = BL.readFile filePath

-- Polymorphic function that takes care of rendering output type
-- a) Data.ByteString.Lazy.ByteString
-- b) Data.Text
renderTodo ::
     (a -> RuntimeMonad ())
  -> (String -> String -> [TodoSection] -> a)
  -> String
  -> String
  -> [TodoSection]
  -> RuntimeMonad ()
renderTodo printer getOutput title' subtitle' =
  printer . getOutput title' subtitle'

renderTodoBare ::
     TexRenderOptions -> String -> String -> [TodoSection] -> BL.ByteString
renderTodoBare opts _ _ = renderTex opts

resolveMustacheCtx ::
     TexRenderOptions -> String -> String -> [TodoSection] -> MustacheCtx
resolveMustacheCtx opts title' subtitle' =
  MustacheCtx title' subtitle' . renderTex opts

renderMustacheTemplate ::
     (T.Text -> IO ()) -> FilePath -> MustacheCtx -> RuntimeMonad ()
renderMustacheTemplate printer templateFp ctx = do
  mustacheContents <- liftIO . BS.readFile $ templateFp
  compiledTemplate <-
    retrieveRight .
    either (Left . show) Right . compileTemplate "" . TE.decodeUtf8 $
    mustacheContents
  let fullText = substitute compiledTemplate ctx
  liftIO . printer $ fullText

main :: IO ()
main = do
  result <-
    runExceptT $ do
      ctx <- liftIO getCtx
      contents <- (liftIO . readInput . input . cliArgs) ctx
      todo <-
        retrieveRight . A.eitherDecode $ contents :: RuntimeMonad [TodoSection]
      let todoWithConditionsResolved = resolveTodoSections ctx todo
      let subtreeAttachmentInputFiles = attachedJsonSubtrees . cliArgs $ ctx
      let subtreeAttachmentBehaviour = resolveSubtreeAttachmentBehaviour ctx
      attachmentInputs <-
        liftIO . forM subtreeAttachmentInputFiles $ \attachmentInput -> do
          attachmentInputContents <- readInput . Just $ attachmentInput
          return . A.eitherDecode $ attachmentInputContents
      subtreeAttachments <- retrieveRight . concatResults $ attachmentInputs
      todoWithSubtrees <-
        retrieveRight .
        attachSubtrees subtreeAttachmentBehaviour todoWithConditionsResolved $
        subtreeAttachments
      let title' = title . cliArgs $ ctx
      let subtitle' = subtitle . cliArgs $ ctx
      let templateFile = resolveMustacheTemplate ctx
      let outputFile = output . cliArgs $ ctx
      let texOpts =
            TexRenderOptions
              { enumerateTag = resolveEnumerateTag ctx
              , itemizeTag = resolveItemizeTag ctx
              }
      case outputFile of
        Nothing ->
          case templateFile of
            Nothing ->
              renderTodo
                (liftIO . BL.putStr)
                (renderTodoBare texOpts)
                title'
                subtitle'
                todoWithSubtrees
            Just tf ->
              renderTodo
                (renderMustacheTemplate TIO.putStr tf)
                (resolveMustacheCtx texOpts)
                title'
                subtitle'
                todoWithSubtrees
        Just f ->
          case templateFile of
            Nothing ->
              renderTodo
                (liftIO . BL.writeFile f)
                (renderTodoBare texOpts)
                title'
                subtitle'
                todoWithSubtrees
            Just tf ->
              renderTodo
                (renderMustacheTemplate (TIO.writeFile f) tf)
                (resolveMustacheCtx texOpts)
                title'
                subtitle'
                todoWithSubtrees
  case result of
    Left err -> putStrLn err
    Right _ -> return ()
