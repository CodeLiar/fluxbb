{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module DBOp.CRUDPost where

import           Import                        hiding (Value, groupBy, on,
                                                update, (=.), (==.))

import           Database.Esqueleto
import           Database.Esqueleto.PostgreSQL

insertPost ::
     (BaseBackend backend ~ SqlBackend, MonadIO m, PersistStoreWrite backend)
  => Key Topics
  -> Int
  -> Text
  -> Key Users
  -> Text
  -> ReaderT backend m (Key Posts)
insertPost tid number username userid content = do
  now <- liftIO getCurrentTime
  let postsTopicId = tid
      postsNumber = number
      postsUsername = username
      postsUserId = userid
      postsTime = now
      postsContent = content
  insert Posts {..}

selectPostByTopicId ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Key Topics
  -> Int64
  -> ReaderT backend m [Entity Posts]
selectPostByTopicId tid page = do
  select $
    from $ \post -> do
      where_ (post ^. PostsTopicId ==. val tid)
      orderBy [asc (post ^. PostsNumber)]
      offset ((page - 1) * 25)
      limit 25
      return post

