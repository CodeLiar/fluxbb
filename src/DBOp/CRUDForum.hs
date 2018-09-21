{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module DBOp.CRUDForum where

import           Import hiding (Value, groupBy, on, (==.))

import           Database.Esqueleto
import           Database.Esqueleto.PostgreSQL

insertForum ::
     (BaseBackend backend ~ SqlBackend, PersistStoreWrite backend, MonadIO m)
  => Key Categories
  -> Text
  -> Maybe Text
  -> ReaderT backend m ()
insertForum cid name desc = do
  insert_ $ Forums cid name desc 0 0 Nothing Nothing Nothing

selectAllForumsAndCategoryName ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => ReaderT backend m [(Text, Maybe [Text], Maybe [Key Forums])]
selectAllForumsAndCategoryName = do
  catnameandforums <-
    select $
    from $ \(category, forum) -> do
      where_ (category ^. CategoriesId ==. forum ^. ForumsCategoryId)
      groupBy (category ^. CategoriesName)
      return
        ( category ^. CategoriesName
        , arrayAgg (forum ^. ForumsName)
        , arrayAgg (forum ^. ForumsId))
  return $
    map (\(a, b, c) -> (unValue a, unValue b, unValue c)) catnameandforums

deleteForumById ::
     ( BaseBackend backend ~ SqlBackend
     , PersistQueryWrite backend
     , MonadIO m
     , BackendCompatible SqlBackend backend
     , PersistUniqueRead backend
     )
  => Key Forums
  -> ReaderT backend m ()
deleteForumById fid = do
  forums <- select $ from $ \forum -> do
    where_ (forum ^. ForumsId ==. val fid)
    return forum
  forM_ forums (deleteCascade . entityKey)

