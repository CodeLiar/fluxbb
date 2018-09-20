{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import           ClassyPrelude.Yesod
import           Database.Persist.TH
import           Yesod.Auth.HashDB

import           Model.Grouping

share
  [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Categories
      name Text
      UniqueCatName name
      deriving Show Eq
    Forums
      categoryId CategoriesId
      name Text
      descriptions Text Maybe
      topicsCount Int default=0
      repliesCount Int default=0
      lastPost UTCTime Maybe
      lastPostId PostsId Maybe
      lastPoster Text Maybe
      deriving Show Eq
    Topics
      forumId ForumsId
      poster Text
      subject Text
      repliesCount Int default=0
      startTime UTCTime
      lastPost UTCTime Maybe
      lastPostId PostsId Maybe
      lastPoster Text Maybe
      isLocked Bool default=false
      deriving Show Eq
    Posts
      topicId TopicsId
      number Int
      username Text
      userId UsersId
      time UTCTime
      content Text
      deriving Show Eq
    Groups
      grouping Grouping
      UniqueGrouping grouping
      deriving Show Eq Read
    Permissions
      groupId GroupsId
      permissionName Text
      banUser Bool
      lockDiscussion Bool
      read Bool
      reply Bool
      createTopic Bool
      deriving Show Eq
    Users
      groupId GroupsId
      username Text
      email Text
      password Text Maybe
      joinTime UTCTime
      topicsStarted Int default=0
      repliesPosted Int default=0
      UniqueUsername username
      UniqueEmail email
      deriving Show Eq
    Banneds
      username Text
      ip Text Maybe
      message Text Maybe
      executor UsersId
      stillInEffect Bool
      deriving Show Eq
    Reports
      postId PostsId
      topicId TopicsId
      forumId ForumsId
      reportedBy UsersId
      created UTCTime
      message Text
      zapped UTCTime Maybe
      zappedBy UsersId Maybe
      deriving Show Eq
  |]

instance HashDBUser Users where
    userPasswordHash = usersPassword
    setPasswordHash h u = u {usersPassword = Just h}
