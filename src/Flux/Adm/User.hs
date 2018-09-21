{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
module Flux.Adm.User where

import           Import             hiding (Value)

import           Database.Esqueleto

import           DBOp.CRUDGroup
import           DBOp.CRUDUser
import           DBOp.CRUDBan
import           Flux.Miscellaneous
import           Flux.User
import           Flux.Adm.Ban

promoteUser ::
     ( YesodPersistBackend (HandlerSite m) ~ SqlBackend
     , YesodPersist (HandlerSite m)
     , BackendCompatible SqlBackend (YesodPersistBackend (HandlerSite m))
     , PersistQueryRead (YesodPersistBackend (HandlerSite m))
     , PersistUniqueRead (YesodPersistBackend (HandlerSite m))
     , MonadHandler m
     )
  => Key Users
  -> Text
  -> Grouping
  -> Key Users
  -> Key Groups
  -> m ()
promoteUser execid execname execgroup targetid targetgroupid = do
  guardUser execid targetid
  guardGroup execgroup
  enttargetuser <- getUserById targetid
  enttargetgroup <- getGroupById targetgroupid
  let username = usersUsername $ entityVal enttargetuser
      group = groupsGrouping $ entityVal enttargetgroup
  liftHandler . runDB $ do
    updateUserGroupingByUsername username Member
    updateBan username Nothing Nothing execid False
    updateUserGroupingByUsername username group
  where
    guardUser xid tid =
      if xid /= tid
        then return ()
        else invalidArgs ["Cannot promote or demote yourself."]
    guardGroup Administrator = return ()
    guardGroup _ = invalidArgs ["Only admin allowed to promote."]

