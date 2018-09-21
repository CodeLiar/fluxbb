module Import.Util
  ( forceTextToInt64
  ) where

import           ClassyPrelude.Yesod

forceTextToInt64 :: Text -> Int64
forceTextToInt64 t =
  case readMay t of
    Just i  -> i :: Int64
    Nothing -> 0

