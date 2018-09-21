{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module DBOp.CRUDGroup where

import           Import                        hiding (Value, groupBy, on,
                                                update, (+=.), (=.), (==.))

import           Database.Esqueleto

selectGroupByGrouping ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Grouping
  -> ReaderT backend m [Entity Groups]
selectGroupByGrouping groupname = do
  select $
    from $ \group -> do
      where_ (group ^. GroupsGrouping ==. val groupname)
      limit 1
      return group

selectGroupByUsername ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Text
  -> ReaderT backend m [(Value (Key Users), Value (Key Groups), Value Grouping)]
selectGroupByUsername username = do
  select $
    from $ \(user, group) -> do
      where_
        (user ^. UsersGroupId ==. group ^. GroupsId
         &&. user ^. UsersUsername ==. val username)
      limit 1
      return (user ^. UsersId, group ^. GroupsId, group ^. GroupsGrouping)

selectGroupByUserId ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Key Users
  -> ReaderT backend m [(Value (Key Users), Value (Key Groups), Value Grouping)]
selectGroupByUserId userid = do
  select $
    from $ \(user, group) -> do
      where_
        (user ^. UsersGroupId ==. group ^. GroupsId
         &&. user ^. UsersId ==. val userid)
      limit 1
      return (user ^. UsersId, group ^. GroupsId, group ^. GroupsGrouping)

selectAllGroups ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => ReaderT backend m [Entity Groups]
selectAllGroups = do
  select $
    from $ \group -> do
      orderBy [asc (group ^. GroupsGrouping)]
      return group

selectGroupByGroupId ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Key Groups
  -> ReaderT backend m [Entity Groups]
selectGroupByGroupId gid = do
  select $
    from $ \group -> do
      where_ (group ^. GroupsId ==. val gid)
      limit 1
      return group

