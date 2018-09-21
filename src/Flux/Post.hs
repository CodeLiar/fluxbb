{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Flux.Post where

import           Import             hiding (Value)

import           Database.Esqueleto
import           DBOp.CRUDPost
import           DBOp.CRUDTopic

getPostsInTopic ::
     ( BackendCompatible SqlBackend (YesodPersistBackend (HandlerSite m))
     , PersistQueryRead (YesodPersistBackend (HandlerSite m))
     , PersistUniqueRead (YesodPersistBackend (HandlerSite m))
     , YesodPersist (HandlerSite m)
     , MonadHandler m
     )
  => Key Topics 
  -> Int64
  -> m [Entity Posts]
getPostsInTopic tid page
  | page < 1 = invalidArgs ["Have you seen something page 0 before?"]
  | otherwise = liftHandler $ runDB $ selectPostByTopicId tid page 
