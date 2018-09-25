{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module DBOp.CRUDTopic where

import           Import                        hiding (Value, groupBy, on,
                                                update, (+=.), (=.), (==.))

import           Database.Esqueleto
import           Database.Esqueleto.PostgreSQL

insertTopic ::
     (BaseBackend backend ~ SqlBackend, MonadIO m, PersistStoreWrite backend)
  => Key Forums
  -> Text
  -> Text
  -> ReaderT backend m (Key Topics)
insertTopic fid poster subject = do
  now <- liftIO getCurrentTime
  let topicsForumId = fid
      topicsPoster = poster
      topicsSubject = subject
      topicsRepliesCount = 0
      topicsStartTime = now
      topicsLastPost = Nothing
      topicsLastPostId = Nothing
      topicsLastPoster = Nothing
      topicsIsLocked = False
  insert Topics {..}

selectTopicById ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Key Topics
  -> ReaderT backend m [Entity Topics]
selectTopicById tid = do
  select $ from $ \topic -> do
    where_ (topic ^. TopicsId ==. val tid)
    limit 1
    return topic

updateTopicIsLocked ::
     MonadIO m => Key Topics -> Bool -> ReaderT SqlBackend m ()
updateTopicIsLocked tid locked = do
  update $ \topic -> do
    set topic [TopicsIsLocked =. val locked]
    where_ (topic ^. TopicsId ==. val tid)

updateTopicIncrementReplyAndLasts ::
     MonadIO m
  => Key Topics
  -> Text
  -> Key Posts
  -> UTCTime
  -> ReaderT SqlBackend m ()
updateTopicIncrementReplyAndLasts tid username pid now = do
  update $ \topic -> do
    set
      topic
      [ TopicsRepliesCount +=. (val 1)
      , TopicsLastPoster =. (val $ Just username)
      , TopicsLastPostId =. (val $ Just pid)
      , TopicsLastPost =. (val $ Just now)
      ]
    where_ (topic ^. TopicsId ==. val tid)

selectTopicForumNameByPosterId ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => Key Users
  -> ReaderT backend m [(Value Text, Entity Topics)]
selectTopicForumNameByPosterId userid = do
  select $
    from $ \(forum `InnerJoin` topic `InnerJoin` user) -> do
      on (topic ^. TopicsPoster ==. user ^. UsersUsername)
      on (topic ^. TopicsForumId ==. forum ^. ForumsId)
      where_ (user ^. UsersId ==. val userid)
      orderBy [asc (topic ^. TopicsLastPost)]
      return (forum ^. ForumsName, topic)

