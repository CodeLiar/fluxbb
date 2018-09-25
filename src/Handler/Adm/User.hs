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
import           Flux.Adm.User

data SearchUserForm = SearchUserForm
  { searchUserFormGroupId  :: Maybe Int64
  , searchUserFormUsername :: Maybe Text
  , searchUserFormEmail    :: Maybe Text
  }

searchUserForm :: [Entity Groups] -> Form SearchUserForm
searchUserForm groups =
  renderDivs $ SearchUserForm
  <$> aopt (selectFieldList $ glist groups) "Groups" Nothing
  <*> aopt textField "Username" Nothing
  <*> aopt textField "Email" Nothing

data BanUsersOptionsForm = BanUsersOptionsForm
  { banUsersOptionsFormMessessage :: Maybe Text
  , banUsersOptionsFormIds        :: Text
  }

banUsersOptionsForm :: Text -> Form BanUsersOptionsForm
banUsersOptionsForm userid =
  renderDivs $ BanUsersOptionsForm
  <$> aopt textField "Message" Nothing
  <*> areq hiddenField "" (Just userid)

data PromoteUsersForm = PromoteUsersForm
  { promoteUsersFormGroupId :: Int64
  , promoteUsersFormUserIds :: Text
  } deriving (Show)

promoteUsersForm :: [Entity Groups] -> Text -> Form PromoteUsersForm
promoteUsersForm groups userids =
  renderDivs $ PromoteUsersForm
  <$> areq (selectFieldList $ glist groups) "Group" Nothing
  <*> areq hiddenField "" (Just userids)

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
      let ban = True
      (wid, enct) <-
        generateFormPost $ banUsersOptionsForm $ intercalate "," userids
      adminLayout uid name group $ do
        setTitle "ban"
        $(widgetFile "adm-user-promote")
    Just "change" -> do
      (uid, name, group) <- allowedToAdmin
      let ban = False
      mo <- getGroup Moderator
      me <- getGroup Member
      (wid, enct) <-
        generateFormPost $
        promoteUsersForm [mo, me] $ intercalate "," userids
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
    Just "promote" -> do
      (uid, name, group) <- allowedToAdmin
      mo <- getGroup Moderator
      me <- getGroup Member
      ((res, _), _) <- runFormPost $ promoteUsersForm [mo, me] ""
      case res of
        FormSuccess r -> do
          let (gid, userids) =
                ( promoteUsersFormGroupId r
                , map forceTextToInt64 $ splitOn "," $ promoteUsersFormUserIds r)
          forM_ userids $ \userid ->
            promoteUser uid name group (toSqlKey userid) (toSqlKey gid)
          redirect AdmUserR
        _ -> invalidArgs ["Please..."]
    _ -> invalidArgs ["Can't understand that."]

