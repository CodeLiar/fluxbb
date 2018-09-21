{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes #-}

module Flux.AdmForum where

import           Import         hiding (Value)

import Database.Esqueleto

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

getForumsAndItsCategory ::
     ( BackendCompatible SqlBackend (YesodPersistBackend (HandlerSite m))
     , PersistQueryRead (YesodPersistBackend (HandlerSite m))
     , PersistUniqueRead (YesodPersistBackend (HandlerSite m))
     , YesodPersist (HandlerSite m)
     , MonadHandler m
     )
  => m [(Text, [(Text, Key Forums)])]
getForumsAndItsCategory = do
  something <- liftHandler $ runDB $ selectAllForumsAndCategoryName
  return $ map (\(cname, mts, mks) -> (cname, spread mts mks)) something
  where
    spread (Just ts) (Just ks) = zip ts ks
    spread (Just _) Nothing    = []
    spread Nothing _           = []

deleteForums ::
     ( BaseBackend (YesodPersistBackend (HandlerSite m)) ~ SqlBackend
     , PersistUniqueRead (YesodPersistBackend (HandlerSite m))
     , BackendCompatible SqlBackend (YesodPersistBackend (HandlerSite m))
     , PersistQueryWrite (YesodPersistBackend (HandlerSite m))
     , YesodPersist (HandlerSite m)
     , MonadHandler m
     )
  => Grouping
  -> [Text]
  -> m ()
deleteForums Administrator fids =
  liftHandler $
  runDB $ forM_ fids (deleteForumById . toSqlKey . forceTextToInt64)
deleteForums _ _ = permissionDenied "You're not allowed to do this (delete forum)"

