{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Foundation where

import Import.NoFoundation      hiding ((&&.), (==.))

import Database.Esqueleto
import Text.Hamlet
import Text.Jasmine

import Yesod.Auth.HashDB
import Yesod.Auth.Message
import Yesod.Core.Types
import Yesod.Default.Util
import Yesod.Form
import Yesod.Static

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
    /static                  StaticR             Static appStatic
    /auth                    SigninR             Auth getAuth
    /profile                 ProfileR            GET
    /admin/category          AdmCategoryR        GET
  |]

type Form a = Html -> MForm (HandlerFor App) (FormResult a, Widget)
type DB a = forall (m:: * -> *). (MonadIO m) => ReaderT SqlBackend m a

instance Yesod App where
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root
    makeSessionBackend _ = Just <$> defaultClientSessionBackend (60 * 5) "config/client-session-key.aes"
    yesodMiddleware = defaultYesodMiddleware
    addStaticContent ext mime content = do
        yes <- getYesod
        let statdir = appStaticDir $ appSettings yes
        addStaticContentExternal
            minifym
            genFilename
            statdir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
        where
            genFilename lbs = "autogen-" ++ base64md5 lbs
    defaultLayout widget = do
        master <- getYesod
        maut <- maybeAuth
        mmessage <- getMessage
        pagecontent <- widgetToPageContent $ do
            addStylesheet $ StaticR css_main_css
            addStylesheet $ StaticR css_milligram_min_css
            addStylesheet $ StaticR css_main_css
            $(widgetFile "def")
        withUrlRenderer $(hamletFile "templates/wrapper.hamlet")
    authRoute _ = Just $ SigninR LoginR
    isAuthorized (SigninR _) _ = return Authorized
    isAuthorized HomeR _       = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized _ _    = isLoggedIn

isLoggedIn :: Handler AuthResult
isLoggedIn = do
    maut <- maybeAuth
    case maut of
        Nothing -> return $ Unauthorized "login please"
        Just _ -> return Authorized

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth App where
    type AuthId App = UsersId
    loginDest _ = HomeR
    logoutDest _ = HomeR
    redirectToReferer _ = False
    authPlugins _ = [authHashDBWithForm loginform (Just . UniqueUsername)]
        where
            loginform :: Route App -> Widget
            loginform action = $(whamletFile "templates/login.hamlet")

    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueUsername $ credsIdent creds
        case x of
            Nothing   -> return $ UserError InvalidLogin
            Just (Entity uid _) -> return $ Authenticated uid

instance YesodAuthPersist App

getUserAndGrouping :: Handler (Maybe (Key Users, Text, Grouping))
getUserAndGrouping = do
  maut <- maybeAuth
  case maut of
    Nothing -> return Nothing
    Just (Entity uid user) -> do
      [gro] <-
        liftHandler $
        runDB $
        select $
        from $ \(group, user) -> do
          where_
            (user ^. UsersId ==. val uid
             &&. group ^. GroupsId ==. user ^. UsersGroupId)
          limit 1
          return (group ^. GroupsGrouping)
      return $ Just (uid, usersUsername user, unValue gro)

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnectionPool master
