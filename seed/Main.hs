{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

import           ClassyPrelude
import           ClassyPrelude.Yesod

import           Control.Monad.Logger
import           Database.Persist.Postgresql
import           Yesod.Auth.Util.PasswordStore
import           Yesod.Default.Config2

import           Foundation
import           Model
import           Model.Grouping
import           Settings

insertGroup txt = insert $ Groups txt

createAdministrator gid username email password = do
  now <- liftIO getCurrentTime
  let usersGroupId = gid
      usersUsername = username
      usersEmail = email
      usersPassword = password
      usersJoinTime = now
      usersTopicsStarted = 0
      usersRepliesPosted = 0
  insert_ $ Users {..}

main :: IO ()
main = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  let conn = (pgConnStr $ appDatabaseConf settings)
  username <- getLine
  password <-
    do something <- getLine
       password <- makePassword (encodeUtf8 something) 17
       return $ decodeUtf8 password
  email <- getLine
  runStderrLoggingT . withPostgresqlConn conn $
    runSqlConn $ do
      runMigration migrateAll
      gid <- insertGroup Administrator
      gin <- insertGroup Moderator
      gim <- insertGroup Member
      gib <- insertGroup Banned
      createAdministrator gid username email (Just password)

