{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Flux.AdmForum where

import           Import

import           DBOp.CRUDForum

createForum ::
     ( BaseBackend (YesodPersistBackend (HandlerSite m)) ~ SqlBackend
     , PersistStoreWrite (YesodPersistBackend (HandlerSite m))
     , YesodPersist (HandlerSite m)
     , MonadHandler m
     )
  => Grouping
  -> Key Categories
  -> Text
  -> Maybe Text
  -> m ()
createForum Administrator cid name desc =
  liftHandler $ runDB $ insertForum cid name desc
createForum _ _ _ _ =
  permissionDenied "You're not allowed to do this (create forum)."

