{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Profile where

import           Foundation

import           Database.Persist.Sql
import           Yesod.Auth
import           Yesod.Core

import           Model

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

