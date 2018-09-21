{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Flux.Home where

import           Import             hiding (Value)

import           Database.Esqueleto

import           DBOp.CRUDCategory

getCategoriesForIndex ::
     ( BackendCompatible SqlBackend (YesodPersistBackend site)
     , PersistQueryRead (YesodPersistBackend site)
     , PersistUniqueRead (YesodPersistBackend site)
     , YesodPersist site
     )
  => HandlerFor site [( Text
                      , [( Key Forums
                         , Text
                         , Maybe Text
                         , Int
                         , Int
                         , Maybe UTCTime
                         , Maybe (Key Posts)
                         , Maybe Text)])]
getCategoriesForIndex = do
  categoriesandforums <- liftHandler $ runDB $ selectCategoriesForIndex
  return $ map anu categoriesandforums
  where
    anu s =
      case s of
        (a, Just b, Just c, Just d, Just e, Just f, Just g, Just h, Just i) ->
          (a, zip8 b c d e f g h i)
        (a, _, _, _, _, _, _, _, _) -> (a, [])

