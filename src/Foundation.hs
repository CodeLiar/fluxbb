{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Foundation where

import ClassyPrelude
import ClassyPrelude.Yesod
import Database.Persist.Sql
import Network.HTTP.Client
import Text.Hamlet
import Yesod.Auth
import Yesod.Auth.HashDB
import Yesod.Auth.Message
import Yesod.Core
import Yesod.Core.Types
import Yesod.Form
import Yesod.Static

import Model
import Settings

data App = App
    { appSettings :: ApplicationSettings
    , appConnectionPool :: ConnectionPool -- from "persistent", module Database.Persist.Sql
    , appLogger :: Logger -- from module Yesod.Core.Types
    , appStatic :: Static -- From "yesod-static", module Yesod.Static
    , appHttpManager :: Manager -- from "http-client", module Network.HTTP.Client
    }

mkYesodData
  "App"
  [parseRoutes|
    /                        HomeR               GET
  |]

type Form a = Html -> MForm (HandlerFor App) (FormResult a, Widget)
type DB a = forall (m:: * -> *). (MonadIO m) => ReaderT SqlBackend m a

instance Yesod App where
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root
    yesodMiddleware = defaultYesodMiddleware
    defaultLayout widget = do
        master <- getYesod
        mmessage <- getMessage
        mcurrentroute <- getCurrentRoute
        pagecontent <- widgetToPageContent $ do
            [whamlet|
                $maybe rout <- mcurrentroute
                    <p>You're at ${show route}.
                $nothing
                    <p>Apparently you're lost.
                ^{widget}
            |]
        withUrlRenderer $(hamletFile "templates/wrapper.hamlet")

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth App where
    type AuthId App = UsersId
    loginDest _ = HomeR
    logoutDest _ = HomeR
    redirectToReferer _ = False
    authPlugins _ = [authHashDB (Just . UniqueUsername)]
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueUsername $ credsIdent creds
        case x of
            Nothing   -> return $ UserError InvalidLogin
            Just (Entity uid _) -> return $ Authenticated uid

instance YesodAuthPersist App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnectionPool master
