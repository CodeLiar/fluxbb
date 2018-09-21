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

selectPostById ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Key Posts
  -> ReaderT backend m [Entity Posts]
selectPostById pid = do
  select $
    from $ \post -> do
      where_ (post ^. PostsId ==. val pid)
      limit 1
      return post

selectPostAndItsParentsInfo ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Key Posts
  -> ReaderT backend m [( Value Text
                        , Value (Key Forums)
                        , Value Text
                        , Entity Posts)]
selectPostAndItsParentsInfo pid = do
  select $
    from $ \(post `InnerJoin` topic `InnerJoin` forum) -> do
      on (forum ^. ForumsId ==. topic ^. TopicsForumId)
      on (topic ^. TopicsId ==. post ^. PostsTopicId)
      where_ (post ^. PostsId ==. val pid)
      limit 1
      return
        ( forum ^. ForumsName
        , forum ^. ForumsId
        , topic ^. TopicsSubject
        , post)

updatePostContent :: MonadIO m => Key Posts -> Text -> ReaderT SqlBackend m ()
updatePostContent pid content = do
  update $ \post -> do
    set post [PostsContent =. val content]
    where_ (post ^. PostsId ==. val pid)


