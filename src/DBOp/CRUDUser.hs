{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module DBOp.CRUDUser where

import           Import             hiding (Value, groupBy, on, update, (+=.),
                                     (=.), (==.), (||.))

import           Database.Esqueleto

import           DBOp.CRUDGroup

insertUser ::
     (BaseBackend backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m)
  => Key Groups
  -> Text
  -> Maybe Text
  -> Text
  -> UTCTime
  -> ReaderT backend m (Key Users)
insertUser usersGroupId usersUsername usersPassword usersEmail usersJoinTime = do
  let usersRepliesPosted = 0
      usersTopicsStarted = 0
  insert $ Users {..}

selectUserById ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Key Users
  -> ReaderT backend m [Entity Users]
selectUserById uid = do
  select $
    from $ \user -> do
      where_ (user ^. UsersId ==. val uid)
      limit 1
      return user

selectUserByUsernameOrEmail ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Text
  -> Text
  -> ReaderT backend m [Entity Users]
selectUserByUsernameOrEmail username email = do
  select $
    from $ \user -> do
      where_
        (user ^. UsersUsername ==. val username ||. user ^. UsersEmail ==.
         val email)
      limit 1
      return user

updateUserGroupingByUsername ::
     MonadIO m => Text -> Grouping -> ReaderT SqlBackend m ()
updateUserGroupingByUsername username grouping = do
  [x] <- selectGroupByGrouping grouping
  update $ \user -> do
    set user [UsersGroupId =. val (entityKey x)]
    where_ (user ^. UsersUsername ==. val username)

