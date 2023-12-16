module Ctx.EnvVariables
  ( Todo2TexEnvVars(..)
  , todo2texEnvs
  , latexEnumerateTagEnvVariable
  , latexItemizeTagEnvVariable
  , mustacheTemplateEnvVariable
  , lookupExact
  , lookupConditionalEnvVariable
  ) where

import Control.Applicative (Alternative((<|>)))
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import System.Environment (getEnvironment)

newtype Todo2TexEnvVars = Todo2TexEnvVars
  { innerEnvVars :: Map.Map String String
  } deriving (Show)

envVariablesPrefix :: String
envVariablesPrefix = "TODOLIST_JSON2TEX_"

latexItemizeTagEnvVariable :: String
latexItemizeTagEnvVariable = envVariablesPrefix ++ "LATEX_ITEMIZE_TAG"

latexEnumerateTagEnvVariable :: String
latexEnumerateTagEnvVariable = envVariablesPrefix ++ "LATEX_ENUMERATE_TAG"

mustacheTemplateEnvVariable :: String
mustacheTemplateEnvVariable = envVariablesPrefix ++ "MUSTACHE_TEMPLATE"

todo2texEnvs :: IO Todo2TexEnvVars
todo2texEnvs = do
  allEnvVars <- getEnvironment
  let matchingVars =
        filter (\(key, _) -> envVariablesPrefix `isPrefixOf` key) allEnvVars
  return . Todo2TexEnvVars . Map.fromList $ matchingVars

lookupExact :: String -> Todo2TexEnvVars -> Maybe String
lookupExact k = Map.lookup k . innerEnvVars

lookupWithOptionalPrefix :: String -> Todo2TexEnvVars -> Maybe String
lookupWithOptionalPrefix k m =
  lookupExact k m <|> lookupExact (envVariablesPrefix ++ k) m

falseValues :: [Maybe String]
falseValues = [Nothing, Just "", Just "0", Just "false"]

lookupConditionalEnvVariable :: String -> Todo2TexEnvVars -> Bool
lookupConditionalEnvVariable k m =
  all (\fv -> fv /= lookupWithOptionalPrefix k m) falseValues
