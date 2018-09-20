{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module DBOp.CRUDCategory where

import           Import

insertCategory ::
     (BaseBackend backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m)
  => Text
  -> ReaderT backend m (Key Categories)
insertCategory catname = do
  insert $ Categories catname

