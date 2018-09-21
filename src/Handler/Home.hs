{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Home where

import Import

import           Database.Esqueleto
import           Flux.Home
import           Flux.Post

getHomeR :: Handler Html
getHomeR = do
  categoriesforindex <- getCategoriesForIndex
  defaultLayout $ do
    setTitle "Index"
    $(widgetFile "home")

