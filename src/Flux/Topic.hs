{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Flux.Topic where

import           Import             hiding (Value)

import           Database.Esqueleto
import           DBOp.CRUDPost
import           DBOp.CRUDTopic

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

