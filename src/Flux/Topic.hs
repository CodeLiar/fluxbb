{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Flux.Topic where

import           Import             hiding (Value)

import           Database.Esqueleto
import           DBOp.CRUDForum
import           DBOp.CRUDPost
import           DBOp.CRUDTopic

import           Handler.User

getTopicById ::
     ( BackendCompatible SqlBackend (YesodPersistBackend (HandlerSite m))
     , PersistQueryRead (YesodPersistBackend (HandlerSite m))
     , PersistUniqueRead (YesodPersistBackend (HandlerSite m))
     , YesodPersist (HandlerSite m)
     , MonadHandler m
     )
  => Key Topics
  -> m (Entity Topics)
getTopicById tid = do
  topics <- liftHandler $ runDB $ selectTopicById tid
  case topics of
    [x] -> return x
    _   -> notFound

replyTopicByPosting ::
     Key Users -> Text -> Key Topics -> Text -> Handler (Key Topics, Int64, Int)
replyTopicByPosting uid uname tid content = do
  now <- liftIO getCurrentTime
  topic <- getTopicById tid
  if topicsIsLocked $ entityVal topic
    then permissionDenied "Topic already locked"
    else do
      let fid = topicsForumId $ entityVal topic
          num = topicsRepliesCount $ entityVal topic
          page = floor $ (toRational num) / 25 + 1 :: Int64
      pid <- liftHandler $ runDB $ insertPost tid (num + 2) uname uid content
      liftHandler $
        runDB $ do
          updateForumIncrementReplyAndLasts
            (topicsForumId $ entityVal topic)
            uname
            pid
            now
          updateTopicIncrementReplyAndLasts tid uname pid now
      return (tid, page, num + 2)

