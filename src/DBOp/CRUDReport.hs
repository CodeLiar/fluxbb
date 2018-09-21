{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module DBOp.CRUDReport where

import           Import                        hiding (Value, groupBy, isNothing, on,
                                                update, (=.), (==.))

import           Database.Esqueleto
import           Database.Esqueleto.PostgreSQL

insertReport ::
     (BaseBackend backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m)
  => Key Posts
  -> Key Topics
  -> Key Forums
  -> Key Users
  -> UTCTime
  -> Text
  -> Maybe UTCTime
  -> Maybe (Key Users)
  -> ReaderT backend m ()
insertReport pid tid fid username now message nothing nothing' = do
  insert_ $ Reports pid tid fid username now message nothing nothing'

selectReportsUnzappedInfo ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => ReaderT backend m [(Entity Reports, Value Text, Value Text, Value Text)]
selectReportsUnzappedInfo = do
  select $
    from $ \(report, user, topic, forum) -> do
      where_
        (report ^. ReportsReportedBy ==. user ^. UsersId
         &&. report ^. ReportsForumId ==. forum ^. ForumsId
         &&. report ^. ReportsTopicId ==. topic ^. TopicsId
         &&. isNothing (report ^. ReportsZappedBy))
      return
        ( report
        , user ^. UsersUsername
        , forum ^. ForumsName
        , topic ^. TopicsSubject)

selectReportsZappedInfo ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => ReaderT backend m [( Entity Reports
                        , Value Text
                        , Value Text
                        , Value Text
                        , Value Text)]
selectReportsZappedInfo = do
  select $
    from $ \(report, user, topic, forum, user') -> do
      where_
        (report ^. ReportsReportedBy ==. user ^. UsersId
         &&. report ^. ReportsForumId ==. forum ^. ForumsId
         &&. report ^. ReportsTopicId ==. topic ^. TopicsId
         &&. report ^. ReportsZappedBy ==. just (user' ^. UsersId))
      orderBy [desc (report ^. ReportsZapped)]
      return
        ( report
        , user ^. UsersUsername
        , forum ^. ForumsName
        , topic ^. TopicsSubject
        , user' ^. UsersUsername)

updateReportZap uid rid = do
  now <- liftIO getCurrentTime
  update $ \report -> do
    set
      report
      [ReportsZappedBy =. just (val uid), ReportsZapped =. just (val now)]
    where_ (report ^. ReportsId ==. val rid)


