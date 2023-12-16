module Ctx.CliArgs
  ( Todo2TexCliArgs(..)
  , todo2texCliArgs
  ) where

import Ctx.EnvVariables
  ( latexEnumerateTagEnvVariable
  , latexItemizeTagEnvVariable
  , mustacheTemplateEnvVariable
  )
import Data.Char (toLower)
import Data.Time (LocalTime(..))
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Options.Applicative
import SubtreeAttachment.LeafNodeAttachmentBehaviour
import Utils.ErrorHandling (maybeToEither)

data Todo2TexCliArgs = Todo2TexCliArgs
  { title :: String
  , subtitle :: String
  , mustacheTemplate :: Maybe String
  , output :: Maybe String
  , date :: Maybe LocalTime
  , attachedJsonSubtrees :: [String]
  , latexEnumerateTagCliOpt :: Maybe String
  , latexItemizeTagCliOpt :: Maybe String
  , leafNodeAttachmentBahaviour :: Maybe LeafNodeAttachmentBehaviour
  , input :: Maybe String
  } deriving (Show)

leafNodeAttachmentBahaviourParser :: Parser LeafNodeAttachmentBehaviour
leafNodeAttachmentBahaviourParser =
  option caseInsensitiveEnum $
  long "leaf-attachment-behaviour" <>
  metavar "<leaf-attachment-behaviour>" <>
  help "Specify the enum value (Fail, Replace_Enumerate, Replace_Itemize)"
  where
    caseInsensitiveEnum =
      eitherReader $ \arg ->
        case map toLower arg of
          "fail" -> Right Fail
          "replace_enumerate" -> Right ReplaceWithEnumerateNode
          "replace_itemize" -> Right ReplaceWithItemizeNode
          _ ->
            Left
              "Invalid value. Use Fail, Replace_Enumerate, or Replace_Itemize)"

parseLocalTime :: String -> Either String LocalTime
parseLocalTime =
  maybeToEither "Invalid date input" .
  parseTimeM True defaultTimeLocale "%Y-%m-%d"

isoDateParser :: Parser LocalTime
isoDateParser =
  option (eitherReader parseLocalTime) $
  long "date" <>
  short 'd' <>
  metavar "<date>" <>
  help "Formatted date in ISO format - `date -I`; default value = tomorrow"

todo2texParser :: Parser Todo2TexCliArgs
todo2texParser =
  Todo2TexCliArgs <$>
  strOption
    (long "title" <>
     short 't' <> metavar "<title>" <> help "Title in the output document") <*>
  strOption
    (long "subtitle" <>
     short 's' <> metavar "<subtitle>" <> help "Subtitle in the output document") <*>
  optional
    (strOption
       (long "mustache-template" <>
        metavar "<mustache-template>" <>
        help
          ("Mustache template for the output; available in env variable $" ++
           mustacheTemplateEnvVariable))) <*>
  optional
    (strOption
       (long "output" <> short 'o' <> metavar "<output>" <> help "Output path")) <*>
  optional isoDateParser <*>
  many
    (strOption
       (long "attach-json-subtree" <>
        metavar "<attach-json-subtree>" <>
        help "Attach JSON document with subtree")) <*>
  optional
    (strOption
       (long "latex-enumerate-tag" <>
        metavar "<latex-enumerate-tag>" <>
        help
          ("Latex tag used in \\begin{enumerate} - custom styling; available in env variable $" ++
           latexEnumerateTagEnvVariable))) <*>
  optional
    (strOption
       (long "latex-itemize-tag" <>
        metavar "<latex-itemize-tag>" <>
        help
          ("Latex tag used in \\begin{itemize} - custom styling; available in env variable $" ++
           latexItemizeTagEnvVariable))) <*>
  optional leafNodeAttachmentBahaviourParser <*>
  optional (strArgument (metavar "<input>"))

todo2texParserInfo :: ParserInfo Todo2TexCliArgs
todo2texParserInfo =
  info
    (todo2texParser <**> helper)
    (fullDesc <>
     progDesc "Render .tex files from .json shaped Todos" <>
     header "todolist-json2tex - tex generator for todos")

todo2texCliArgs :: IO Todo2TexCliArgs
todo2texCliArgs = execParser todo2texParserInfo
