{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module DBOp.CRUDReport where

import           Import                        hiding (Value, groupBy, on,
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

