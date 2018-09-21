{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Adm.User where

import           Import

import           Data.Text          (splitOn)
import           Database.Esqueleto

import           Flux.Miscellaneous
import           Flux.User
import           Flux.Adm.Ban

data SearchUserForm = SearchUserForm
  { searchUserFormGroupId  :: Maybe Int64
  , searchUserFormUsername :: Maybe Text
  , searchUserFormEmail    :: Maybe Text
  }

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

data BanUsersOptionsForm = BanUsersOptionsForm
  { banUsersOptionsFormMessessage :: Maybe Text
  , banUsersOptionsFormIds        :: Text
  }

banUsersOptionsForm :: Text -> Form BanUsersOptionsForm
banUsersOptionsForm userid =
  renderDivs $ BanUsersOptionsForm
  <$> aopt textField "Message" Nothing
  <*> areq hiddenField "" (Just userid)

groupToHtml :: Grouping -> Html
groupToHtml = toHtml . show

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

postAdmUserPromoteR :: Handler Html
postAdmUserPromoteR = do
  promote <- lookupPostParam "promote"
  userids <- lookupPostParams "user-id"
  case promote of
    Just "ban" -> do
      (uid, name, group) <- allowedToMod
      (wid, enct) <-
        generateFormPost $ banUsersOptionsForm $ intercalate "," userids
      adminLayout uid name group $ do
        setTitle "Promote"
        $(widgetFile "adm-user-promote")
    _ -> invalidArgs ["What do you want to do with the users?"]

postAdmUserPromoteExeR :: Handler Html
postAdmUserPromoteExeR = do
  promote <- lookupPostParam "promote"
  case promote of
    Nothing -> invalidArgs ["No promote param."]
    Just "ban" -> do
      (uid, name, group) <- allowedToMod
      ((res, wid), enct) <- runFormPost $ banUsersOptionsForm ""
      case res of
        FormSuccess r -> do
          let (mes, userids) =
                ( banUsersOptionsFormMessessage r
                , map forceTextToInt64 $ splitOn "," $ banUsersOptionsFormIds r)
          forM_ userids $ \userid ->
            banUserById uid name group (toSqlKey userid) Nothing mes
          redirect $ AdmUserR
    Just "change" -> do
      error "Change"
    _ -> invalidArgs ["Can't understand that."]

