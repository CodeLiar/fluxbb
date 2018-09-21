{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.User where

import           Import

import           Data.Time.LocalTime
import           Database.Persist.Sql

import           Flux.User

data RegisterForm = RegisterForm
  { registerFormUsername :: Text
  , registerFormPassword :: Text
  , registerFormEmail    :: Text
  }

registerForm :: Form RegisterForm
registerForm =
  renderDivs $ RegisterForm
  <$> areq textField "Username" Nothing
  <*> areq passwordField "Password" Nothing
  <*> areq emailField "Email" Nothing

getRegisterR :: Handler Html
getRegisterR = do
  isNotLoggedIn
  (wid, enct) <- generateFormPost registerForm
  defaultLayout $(widgetFile "register")

postRegisterR :: Handler Html
postRegisterR = do
  isNotLoggedIn
  ((res, wid), enct) <- runFormPost registerForm
  case res of
    FormSuccess r -> do
      _ <-
        registerUser
          (registerFormUsername r)
          (registerFormPassword r)
          (registerFormEmail r)
      redirect HomeR
    _ ->
      invalidArgs
        [ "Your input doesn't contribute sufficiently"
        , " For this capitalistic society. Think about it."
        ]

getProfileR :: Handler Html
getProfileR = do
  (ruid, name, group) <- allowedToPost
  user'@(Entity uid' user) <- getUserById  ruid
  profileLayout ruid name group user' $(widgetFile "profile-info")

getUserR :: Int64 -> Handler Html
getUserR uid = do
  (ruid, name, group) <- allowedToPost
  user'@(Entity uid' user) <- getUserById $ toSqlKey uid
  profileLayout ruid name group user' $(widgetFile "profile-info")


