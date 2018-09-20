{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Settings.StaticFiles where

import           Yesod.Static

import           Settings

staticFiles (appStaticDir compileTimeAppSettings)

