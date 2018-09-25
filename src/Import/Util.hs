module Import.Util
  ( forceTextToInt64
  , zip8
  , timeZone
  , glist
  ) where

import           ClassyPrelude.Yesod hiding ((.), map)
import           Data.Time.LocalTime
import           Database.Esqueleto  (fromSqlKey)

import           Model               (groupsGrouping)

forceTextToInt64 :: Text -> Int64
forceTextToInt64 t =
  case readMay t of
    Just i  -> i :: Int64
    Nothing -> 0

zip8 ::
     [a]
  -> [b]
  -> [c]
  -> [d]
  -> [e]
  -> [f]
  -> [g]
  -> [h]
  -> [(a, b, c, d, e, f, g, h)]
zip8 = zipWith8 (,,,,,,,)

zipWith8 ::
     (t -> t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> a)
  -> [t]
  -> [t1]
  -> [t2]
  -> [t3]
  -> [t4]
  -> [t5]
  -> [t6]
  -> [t7]
  -> [a]
zipWith8 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) =
  z a b c d e f g h : zipWith8 z as bs cs ds es fs gs hs
zipWith8 z _ _ _ _ _ _ _ _ = []

timeZone :: TimeZone
timeZone = TimeZone 420 False "Jakarta"

glist gs =
  map
    (\x ->
       (pack . show . groupsGrouping $ entityVal x, fromSqlKey . entityKey $ x))
    gs :: [(Text, Int64)]

