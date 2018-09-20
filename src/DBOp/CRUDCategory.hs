{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module DBOp.CRUDCategory where

import           Import                 hiding ((==.))
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
selectAllCategory = do
  select $
    from $ \category -> do
        orderBy [asc (category ^. CategoriesName)]
        return category

deleteCategory ::
     ( BaseBackend backend ~ SqlBackend
     , PersistQueryWrite backend
     , BackendCompatible SqlBackend backend
     , PersistUniqueRead backend
     , MonadIO m
     )
  => Key Categories
  -> ReaderT backend m ()
deleteCategory cid = do
  cat <-
    select $
    from $ \category -> do
      where_ (category ^. CategoriesId ==. val cid)
      return category
  forM_ cat (deleteCascade . entityKey)

