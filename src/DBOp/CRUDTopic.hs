{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module DBOp.CRUDTopic where

import           Import                        hiding (Value, groupBy, on,
                                                update, (=.), (==.))

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

selectTopicById tid = do
  select $ from $ \topic -> do
    where_ (topic ^. TopicsId ==. val tid)
    limit 1
    return topic

updateTopicIsLocked tid locked = do
  update $ \topic -> do
    set topic [TopicsIsLocked =. val locked]
    where_ (topic ^. TopicsId ==. val tid)

