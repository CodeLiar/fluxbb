{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Flux.AdmCategory where

import           Import

import           DBOp.CRUDCategory

createCategory ::
     ( BaseBackend (YesodPersistBackend (HandlerSite m)) ~ SqlBackend
     , PersistStoreWrite (YesodPersistBackend (HandlerSite m))
     , YesodPersist (HandlerSite m)
     , MonadHandler m
     )
  => Grouping
  -> Text
  -> m (Key Categories)
createCategory Administrator catname =
  liftHandler $ runDB $ insertCategory catname
createCategory _ _ =
  permissionDenied "You're not allowed to do this (category creation)."

getAllCategories ::
     ( BackendCompatible SqlBackend (YesodPersistBackend site)
     , PersistQueryRead (YesodPersistBackend site)
     , PersistUniqueRead (YesodPersistBackend site)
     , YesodPersist site
     )
  => HandlerFor site [Entity Categories]
getAllCategories = liftHandler $ runDB $ selectAllCategory


