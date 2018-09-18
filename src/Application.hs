{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Control.Monad
import Control.Monad.Logger
import Database.Persist.Postgresql
import Foundation
import Language.Haskell.TH.Syntax
import Network.HTTP.Client.TLS
import Network.Wai.Handler.Warp
import System.Log.FastLogger
import Yesod.Core
import Yesod.Default.Config2
import Yesod.Static

import Home
import Settings     (ApplicationSettings (..),
                     configSettingsYmlValue)

mkYesodDispatch "App" resourcesApp

warpSettings :: App -> Settings -- from module Network.Wai.Handler.Warp
warpSettings app =
    setPort (appPort $ appSettings app) $ -- 1
    setHost (appHost $ appSettings app) $ -- 2
    setOnException
        (\_req exception ->
            when (defaultShouldDisplayException exception) $ --- ^ from Control.Monad ^ from Network.War.Handler.Warp
            messageLoggerSource
                app
                (appLogger app) -- 3
                $(qLocation >>= liftLoc) -- from "template-haskell", module Language.Haskell.TH.Syntax
                "yesod"
                LevelError
                (toLogStr $ "Exception from warp" ++ show exception))
    defaultSettings -- from Network.Wai.Handler.Warp

makeApplication :: App -> IO Application
makeApplication app = do
    commonapp<- toWaiApp app
    return $ defaultMiddlewaresNoLogging commonapp

newMain :: IO ()
newMain = do
    settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
    app <- makeFoundation settings
    commonapp <- makeApplication app
    runSettings (warpSettings app) commonapp

{-
makeFoundation :: ApplicationSettings -> IO App
makeFoundation appSettings = do
    appHttpManager <- getGlobalManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings
        then staticDevel
        else static)
        (appStaticDir appSettings)
    let mkFoundation appConnectionPool = App {..}
        tempFoundation =
            mkFoundation $ error "Connection pool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger
    pool <-
        flip runLoggingT logFunc $
        createPostgresqlPool
        (pgConnStr $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)
    return $ mkFoundation pool

-}

makeFoundation :: ApplicationSettings -> IO App
makeFoundation appSettings = do
    appHttpManager <- getGlobalManager --from package "http-client-tls" , module Network.HTTP.Client.TLS
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger -- from "fast-logger", module System.Log.FastLogger
    appStatic <-
        (if appMutableStatic appSettings
        then staticDevel -- from "yesod-static", module Yesod.Static
        else static) -- from "yesod-static", module Yesod.Static
        (appStaticDir appSettings)
    let mkFoundation appConnectionPool = App {..}
        tempFoundation =
            mkFoundation $ error "Connection pool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    pool <-
        flip runLoggingT logFunc $
        createPostgresqlPool -- from "persistent-postgresql", module Database.Persist.Postgresql
            (pgConnStr $ appDatabaseConf appSettings)
            (pgPoolSize $ appDatabaseConf appSettings)
    return $ mkFoundation pool

