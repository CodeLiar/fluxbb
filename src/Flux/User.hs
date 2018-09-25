{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Flux.User where

import           Import

import           Database.Esqueleto
import           Yesod.Auth.Util.PasswordStore

import           DBOp.CRUDGroup
import           DBOp.CRUDUser

import           Flux.Miscellaneous

unusedUser ::
     ( BackendCompatible SqlBackend (YesodPersistBackend (HandlerSite m))
     , PersistQueryRead (YesodPersistBackend (HandlerSite m))
     , PersistUniqueRead (YesodPersistBackend (HandlerSite m))
     , YesodPersist (HandlerSite m)
     , MonadHandler m
     )
  => Text
  -> Text
  -> m ()
unusedUser username email = do
  users <- liftHandler $ runDB $ selectUserByUsernameOrEmail username email
  case users of
    [] -> return ()
    _  -> invalidArgs ["Username and/or email has been used."]

getUserById ::
     ( BackendCompatible SqlBackend (YesodPersistBackend (HandlerSite m))
     , PersistQueryRead (YesodPersistBackend (HandlerSite m))
     , PersistUniqueRead (YesodPersistBackend (HandlerSite m))
     , YesodPersist (HandlerSite m)
     , MonadHandler m
     )
  => Key Users
  -> m (Entity Users)
getUserById uid = do
  users <- liftHandler $ runDB $ selectUserById uid
  case users of
    []  -> notFound
    x:_ -> return x

registerUser ::
     ( BaseBackend (YesodPersistBackend (HandlerSite m)) ~ SqlBackend
     , PersistStoreWrite (YesodPersistBackend (HandlerSite m))
     , MonadHandler m
     , YesodPersist (HandlerSite m)
     , PersistUniqueRead (YesodPersistBackend (HandlerSite m))
     , PersistQueryRead (YesodPersistBackend (HandlerSite m))
     , BackendCompatible SqlBackend (YesodPersistBackend (HandlerSite m))
     )
  => Text
  -> Text
  -> Text
  -> m (Key Users)
registerUser username password email = do
  unusedUser username email
  now <- liftIO getCurrentTime
  gids <- liftHandler $ runDB $ selectGroupByGrouping Member
  case gids of
    []  -> invalidArgs ["Call the Administrator"]
    x:_ -> do
      password' <- liftIO $ makePassword (encodeUtf8 password) 17
      liftHandler $
        runDB $
        insertUser
          (entityKey x)
          username
          (Just $ decodeUtf8 password')
          email
          now

getUsersByConditions mgid musername memail = do
  userandgroup <-
    liftHandler $ runDB $ selectUsersByConditions mgid musername memail
  return $ map (\(user, Value group) -> (user, group)) userandgroup

getAllUsers ::
     ( BackendCompatible SqlBackend (YesodPersistBackend (HandlerSite m))
     , PersistQueryRead (YesodPersistBackend (HandlerSite m))
     , PersistUniqueRead (YesodPersistBackend (HandlerSite m))
     , YesodPersist (HandlerSite m)
     , MonadHandler m
     )
  => Bool
  -> m [(Grouping, Entity Users)]
getAllUsers ascending = do
  groupandusers <- liftHandler $ runDB $ selectAllUsers ascending
  return $ map (\(Value a, x) -> (a, x)) groupandusers

searchUserByConditions username groupid orderby ascending = do
  groupandusers <- liftHandler $ runDB $ selectUsersBySearchConditions username groupid orderby ascending
  return $ map (\(Value a, x) -> (a, x)) groupandusers

