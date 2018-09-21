{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Adm.User where

import           Import

import           Database.Esqueleto

import           Flux.Miscellaneous
import           Flux.User

data SearchUserForm = SearchUserForm
  { searchUserFormGroupId  :: Maybe Int64
  , searchUserFormUsername :: Maybe Text
  , searchUserFormEmail    :: Maybe Text
  }

groupToHtml :: Grouping -> Html
groupToHtml = toHtml . show

searchUserForm :: [Entity Groups] -> Form SearchUserForm
searchUserForm groups =
  renderDivs $ SearchUserForm
  <$> aopt (selectFieldList glist) "Groups" Nothing
  <*> aopt textField "Username" Nothing
  <*> aopt textField "Email" Nothing
  where
    glist :: [(Text, Int64)]
    glist =
      map
        (\x ->
           ( pack . show . groupsGrouping $ entityVal x
           , fromSqlKey . entityKey $ x))
        groups

getAdmUserR :: Handler Html
getAdmUserR = do
  (uid, name, group) <- allowedToMod
  groups <- getAllGroups
  (wid, enct) <- generateFormPost $ searchUserForm groups
  let users = []
  adminLayout uid name group $ do
    setTitle "Manage Users"
    $(widgetFile "adm-user")

postAdmUserR :: Handler Html
postAdmUserR = do
  (uid, name, group) <- allowedToMod
  groups <- getAllGroups
  ((res, wid), enct) <- runFormPost $ searchUserForm groups
  let (mgid, musername, memail) =
        case res of
          FormSuccess r ->
            ( toSqlKey <$> searchUserFormGroupId r
            , searchUserFormUsername r
            , searchUserFormEmail r)
          _ -> (Nothing, Nothing, Nothing)
  users <- getUsersByConditions mgid musername memail
  adminLayout uid name group $ do
    setTitle "Users"
    $(widgetFile "adm-user")

