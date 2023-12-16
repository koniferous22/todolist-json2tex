module ErrorHandling
  ( RuntimeMonad
  , retrieveRight
  , concatResults
  ) where

import Control.Monad.Except (ExceptT, throwError)

type RuntimeMonad = ExceptT String IO

retrieveRight :: Either String a -> RuntimeMonad a
retrieveRight (Left err) = throwError err
retrieveRight (Right val) = return val

concatResults :: [Either String [a]] -> Either String [a]
concatResults xs = concat <$> sequence xs
