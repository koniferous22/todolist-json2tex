module Utils.ErrorHandling where

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither errMessage Nothing = Left errMessage
maybeToEither _ (Just val) = Right val
