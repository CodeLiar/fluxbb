{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module DBOp.CRUDPost where

import           Import                        hiding (Value, groupBy, on,
                                                (==.))

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

