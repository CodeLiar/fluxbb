{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module DBOp.CRUDBan where

import           Import             hiding (Value, groupBy, on, update, (+=.),
                                     (=.), (==.), (||.))

import           Database.Esqueleto

insertBan ::
     (BaseBackend backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m)
  => Text
  -> Maybe Text
  -> Maybe Text
  -> Key Users
  -> ReaderT backend m ()
insertBan bannedsUsername bannedsIp bannedsMessage bannedsExecutor =
  let bannedsStillInEffect = True
  in insert_ Banneds {..}

selectAllBanneds ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => ReaderT backend m [(Value (Key Users), Entity Banneds, Value Text)]
selectAllBanneds = do
  select $
    from $ \(user, banned, exec) -> do
      where_
        (user ^. UsersUsername ==. banned ^. BannedsUsername
         &&. banned ^. BannedsExecutor ==. exec ^. UsersId
         &&. banned ^. BannedsStillInEffect ==. val True)
      return (user ^. UsersId, banned, exec ^. UsersUsername)

updateBan username ip message exec status = do
  update $ \banned -> do
    set
      banned
      [ BannedsIp =. val ip
      , BannedsMessage =. val message
      , BannedsExecutor =. val exec
      , BannedsStillInEffect =. val status
      ]
    where_ (banned ^. BannedsUsername ==. val username)

