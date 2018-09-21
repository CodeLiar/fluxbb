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
  } deriving (Show)

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
        , " for this capitalistic society. Think about it."
        ]

getProfileR :: Handler Html
getProfileR = do
  (Just (Entity userid user)) <- maybeAuth
  defaultLayout $ do
    setTitle "Nice"
    [whamlet|
      <p> You are: #{usersUsername user}
      <p> Userid: #{fromSqlKey userid}
      <p> Your email: #{usersEmail user}
    |]

getUserR :: Int64 -> Handler Html
getUserR uid = do
  (ruid, name, group) <- allowedToPost
  (Entity uid' user ) <- getUserById $ toSqlKey uid
  defaultLayout $(widgetFile "profile")

