module Ctx
  ( Ctx(..)
  , getCtx
  , resolveEnumerateTag
  , resolveItemizeTag
  , resolveLocalTime
  , resolveDayOfWeek
  , resolveMustacheTemplate
  , resolveSubtreeAttachmentBehaviour
  , resolveConditionalEnvVariable
  ) where

import Control.Applicative (Alternative((<|>)))
import Ctx.CliArgs (Todo2TexCliArgs(..), todo2texCliArgs)
import Ctx.EnvVariables
  ( Todo2TexEnvVars(..)
  , latexEnumerateTagEnvVariable
  , latexItemizeTagEnvVariable
  , lookupConditionalEnvVariable
  , lookupExact
  , mustacheTemplateEnvVariable
  , todo2texEnvs
  )
import Data.Maybe (fromMaybe)
import Data.Time
  ( DayOfWeek(..)
  , LocalTime(..)
  , NominalDiffTime(..)
  , ZonedTime(..)
  , addLocalTime
  , dayOfWeek
  , getZonedTime
  , localDay
  , localTimeOfDay
  , nominalDay
  , zonedTimeToLocalTime
  )
import SubtreeAttachment.AttachmentBehaviour
  ( TextNodeAttachmentBehaviour(..)
  )

data Ctx = Ctx
  { envVars :: Todo2TexEnvVars
  , cliArgs :: Todo2TexCliArgs
  -- 'ZonedTime' = 'LocalTime' + timezone
  , zonedTime :: ZonedTime
  } deriving (Show)

shiftZonedTime :: NominalDiffTime -> ZonedTime -> ZonedTime
shiftZonedTime diff (ZonedTime lt zone) =
  let nextTime = addLocalTime diff lt
   in ZonedTime (LocalTime (localDay nextTime) (localTimeOfDay nextTime)) zone

getCtx :: IO Ctx
getCtx =
  let shiftedZonedTime =
        shiftZonedTime (nominalDay :: NominalDiffTime) <$> getZonedTime
   in Ctx <$> todo2texEnvs <*> todo2texCliArgs <*> shiftedZonedTime

resolveEnumerateTag :: Ctx -> String
resolveEnumerateTag ctx =
  fromMaybe "enumerate" $
  (latexEnumerateTagCliOpt . cliArgs $ ctx) <|>
  lookupExact latexEnumerateTagEnvVariable (envVars ctx)

resolveItemizeTag :: Ctx -> String
resolveItemizeTag ctx =
  fromMaybe "itemize" $
  (latexItemizeTagCliOpt . cliArgs $ ctx) <|>
  lookupExact latexItemizeTagEnvVariable (envVars $ ctx)

-- 'LocalTime' = time without consideration of timezones
resolveLocalTime :: Ctx -> LocalTime
resolveLocalTime ctx =
  fromMaybe ((zonedTimeToLocalTime . zonedTime) ctx) . date . cliArgs $ ctx

resolveDayOfWeek :: Ctx -> DayOfWeek
resolveDayOfWeek = dayOfWeek . localDay . resolveLocalTime

resolveSubtreeAttachmentBehaviour :: Ctx -> TextNodeAttachmentBehaviour
resolveSubtreeAttachmentBehaviour =
  fromMaybe Fail . textNodeAttachmentBahaviour . cliArgs

resolveMustacheTemplate :: Ctx -> Maybe String
resolveMustacheTemplate ctx =
  (mustacheTemplate . cliArgs) ctx <|>
  (lookupExact mustacheTemplateEnvVariable . envVars) ctx

resolveConditionalEnvVariable :: Ctx -> String -> Bool
resolveConditionalEnvVariable (Ctx envVars' _ _) var =
  lookupConditionalEnvVariable var envVars'
