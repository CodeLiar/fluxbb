{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Control.Monad
import Control.Monad.Logger
import Foundation
import Language.Haskell.TH.Syntax
import Network.Wai.Handler.Warp
import Yesod.Core

import Home
import Settings     (ApplicationSettings (..))

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
    let app = undefined
    commonapp <- makeApplication app
    runSettings (warpSettings app) commonapp

