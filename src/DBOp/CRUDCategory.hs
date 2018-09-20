{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module DBOp.CRUDCategory where

import           Import
import           Database.Esqueleto

insertCategory ::
     (BaseBackend backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m)
  => Text
  -> ReaderT backend m (Key Categories)
insertCategory catname = do
  insert $ Categories catname

selectAllCategory ::
     ( PersistUniqueRead b
     , PersistQueryRead b
     , BackendCompatible SqlBackend b
     , MonadIO m
     )
  => ReaderT b m [Entity Categories]
selectAllCategory =
  select $
  from $ \category -> do
    orderBy [asc (category ^. CategoriesName)]
    return category

