{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# Language QuasiQuotes       #-}

module Foundation where

import Database.Persist.Sql
import Network.HTTP.Client
import Yesod.Core
import Yesod.Core.Types
import Yesod.Static

import Settings

data App = App
    { appSettings :: ApplicationSettings
    , appConnectionPool :: ConnectionPool -- from "persistent", module Database.Persist.Sql
    , appLogger :: Logger -- from module Yesod.Core.Types
    , appStatic :: Static -- From "yesod-static", module Yesod.Static
    , appHttpManager :: manager -- from "http-client", module Network.HTTP.Client
    }

mkYesodData
  "App"
  [parseRoutes|
    /                        HomeR               GET
  |]


instance Yesod App where
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root
