{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Flux.Miscellaneous where

import Import

import DBOp.CRUDGroup

getAllGroups ::
     ( BackendCompatible SqlBackend (YesodPersistBackend (HandlerSite m))
     , PersistQueryRead (YesodPersistBackend (HandlerSite m))
     , PersistUniqueRead (YesodPersistBackend (HandlerSite m))
     , YesodPersist (HandlerSite m)
     , MonadHandler m
     )
  => m [Entity Groups]
getAllGroups = liftHandler $ runDB $ selectAllGroups

getGroup ::
     ( BackendCompatible SqlBackend (YesodPersistBackend (HandlerSite m))
     , PersistQueryRead (YesodPersistBackend (HandlerSite m))
     , PersistUniqueRead (YesodPersistBackend (HandlerSite m))
     , YesodPersist (HandlerSite m)
     , MonadHandler m
     )
  => Grouping
  -> m (Entity Groups)
getGroup x = do
  a <- liftHandler . runDB . selectGroupByGrouping $ x
  case a of
    [g] -> return g
    _   -> invalidArgs ["Group not found."]

getGroupById ::
     ( BackendCompatible SqlBackend (YesodPersistBackend (HandlerSite m))
     , PersistQueryRead (YesodPersistBackend (HandlerSite m))
     , PersistUniqueRead (YesodPersistBackend (HandlerSite m))
     , YesodPersist (HandlerSite m)
     , MonadHandler m
     )
  => Key Groups
  -> m (Entity Groups)
getGroupById gid = do
  a <- liftHandler . runDB . selectGroupByGroupId $ gid
  case a of
    [g] -> return g
    _   -> invalidArgs ["Group not found."]

