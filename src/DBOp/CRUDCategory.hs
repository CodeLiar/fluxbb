{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module DBOp.CRUDCategory where

import           Import                 hiding (Value, groupBy, (==.))
import           Database.Esqueleto
import           Database.Esqueleto.PostgreSQL

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

selectCategoriesForIndex ::
     ( PersistUniqueRead backend
     , PersistQueryRead backend
     , BackendCompatible SqlBackend backend
     , MonadIO m
     )
  => ReaderT backend m [( Text
                        , Maybe [Key Forums]
                        , Maybe [Text]
                        , Maybe [Maybe Text]
                        , Maybe [Int]
                        , Maybe [Int]
                        , Maybe [Maybe UTCTime]
                        , Maybe [Maybe (Key Posts)]
                        , Maybe [Maybe Text])]
selectCategoriesForIndex = do
  catnameandforums <-
    select $
    from $ \(category, forum) -> do
      where_ (category ^. CategoriesId ==. forum ^. ForumsCategoryId)
      groupBy (category ^. CategoriesName)
      return
        ( category ^. CategoriesName
        , arrayAgg (forum ^. ForumsId)
        , arrayAgg (forum ^. ForumsName)
        , arrayAgg (forum ^. ForumsDescriptions)
        , arrayAgg (forum ^. ForumsTopicsCount)
        , arrayAgg (forum ^. ForumsRepliesCount)
        , arrayAgg (forum ^. ForumsLastPost)
        , arrayAgg (forum ^. ForumsLastPostId)
        , arrayAgg (forum ^. ForumsLastPoster))
  return $
    map
      (\(a, b, c, d, e, f, g, h, i) ->
         ( unValue a
         , unValue b
         , unValue c
         , unValue d
         , unValue e
         , unValue f
         , unValue g
         , unValue h
         , unValue i))
      catnameandforums

