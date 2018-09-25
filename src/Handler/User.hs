{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.User where

import           Import

import           Data.Time.LocalTime
import           Database.Persist.Sql

import           Flux.Miscellaneous
import           Flux.User
import           Flux.Adm.User
import           Flux.Adm.Ban


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

data SearchUserForm = SearchUserForm
  { searchUserFormUsername  :: Maybe Text
  , searchUserFormGroup     :: Maybe Int64
  , searchUserFormSortBy    :: SortBy
  , searchUserFormAscending :: Bool
  }

searchUserForm :: [Entity Groups] -> Form SearchUserForm
searchUserForm groups = renderDivs $
  SearchUserForm
  <$> aopt textField "Username" Nothing
  <*> aopt (selectFieldList $ glist groups) "Groups" Nothing
  <*> areq (selectFieldList slist) "Sort By" Nothing
  <*> areq (selectFieldList alist) "Sort Order" Nothing
  where
    slist :: [(Text, SortBy)]
    slist = map (pack . show &&& id) [minBound .. maxBound]
    alist :: [(Text, Bool)]
    alist = [("Ascending", True), ("Descending", False)]

data EditByUserForm = EditByUserForm
  { editByUserFormOldPass :: Maybe Text
  , editByUserFormNewPass :: Maybe Text
  , editByUserFormEmail   :: Text
  }

editByUserForm :: Text -> Form EditByUserForm
editByUserForm email = renderDivs $
  EditByUserForm
  <$> aopt passwordField "Old Password" Nothing
  <*> aopt passwordField "New Password" Nothing
  <*> areq emailField "Email" (Just email)

data EditByAdminForm = EditByAdminForm
  { editByAdminFormNewPass :: Maybe Text
  , editByAdminFormEmail   :: Text
  }

editByAdminForm :: Text -> Form EditByAdminForm
editByAdminForm email = renderDivs $
  EditByAdminForm
  <$> aopt passwordField "New Password" Nothing
  <*> areq emailField "Email" (Just email)

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
  (uid, _, _) <- allowedToPost
  redirect $ UserR $ fromSqlKey uid


getUserR :: Int64 -> Handler Html
getUserR uid = do
  (ruid, name, group) <- allowedToPost
  user'@(Entity uid' user) <- getUserById $ toSqlKey uid
  profileLayout ruid name group user' $(widgetFile "profile-info")

getUserListR :: Handler Html
getUserListR = do
  (uid, name, group) <- allowedToPost
  let users = [] :: [(Grouping, Entity Users)]
  ad <- getGroup Administrator
  mo <- getGroup Moderator
  me <- getGroup Member
  (wid, enct) <- generateFormPost $ searchUserForm [ad, mo, me]
  defaultLayout $ do
    setTitle "User List"
    $(widgetFile "user-list")

postUserListR :: Handler Html
postUserListR = do
  (uid, name, group) <- allowedToPost
  ad <- getGroup Administrator
  mo <- getGroup Moderator
  me <- getGroup Member
  ((res, wid), enct) <- runFormPost $ searchUserForm [ad, mo, me]
  case res of
    FormSuccess r -> do
      let (username, groupid, orderby, ascending) =
            ( searchUserFormUsername r
            , toSqlKey <$> searchUserFormGroup r
            , searchUserFormSortBy r
            , searchUserFormAscending r)
      users <- searchUserByConditions username groupid orderby ascending
      defaultLayout $ do
        setTitle "User List"
        $(widgetFile "user-list")
    _ -> error ""

allowedToActuallyEdit :: Int64 -> HandlerFor App (Key Users, Text, Grouping)
allowedToActuallyEdit pid = do
  (uid, name, group) <- allowedToPost
  if uid == toSqlKey pid || group == Administrator
    then return (uid, name, group)
    else permissionDenied "You're not allowed to edit user's information."

generateFormEdit :: Grouping -> Text -> Handler (Widget, Enctype)
generateFormEdit Administrator = generateFormPost . editByAdminForm
generateFormEdit _             = generateFormPost . editByUserForm

getUserEditR :: Int64 -> Handler Html
getUserEditR userid = do
  (uid, name, group) <- allowedToActuallyEdit userid
  user'@(Entity uid' user) <- getUserById $ toSqlKey userid
  (wid, enct) <- generateFormEdit group (usersEmail user)
  profileLayout uid name group user' $(widgetFile "profile-info-edit")

postUserEditR :: Int64 -> Handler Html
postUserEditR userid = do
  (uid, name, group) <- allowedToActuallyEdit userid
  user'@(Entity uid' user) <- getUserById $ toSqlKey userid
  case group of
    Administrator -> do
      ((res, wid), enct) <- runFormPost $ editByAdminForm (usersEmail user)
      case res of
        FormSuccess r -> do
          let (newpass, email) =
                (editByAdminFormNewPass r, editByAdminFormEmail r)
          updateInfoByAdmin uid' newpass email
          redirect $ UserR userid
        _ -> do
          profileLayout uid name group user' $ do
            [whamlet|Please fill the input correctly|]
    _ -> do
      ((res, wid), enct) <- runFormPost $ editByUserForm (usersEmail user)
      case res of
        FormSuccess r -> do
          let (oldpass, newpass, email) =
                ( editByUserFormOldPass r
                , editByUserFormNewPass r
                , editByUserFormEmail r)
          selfUpdateInfoByUser uid (usersPassword user) oldpass newpass email
          redirect $ UserR userid
        _ -> do
          profileLayout uid name group user' $ do
            [whamlet|Please fill the input correctly|]

data PromoteForm = PromoteForm
  { promoteFormGroupId :: Int64
  }

promoteForm :: [Entity Groups] -> Form PromoteForm
promoteForm groups =
  renderDivs $ PromoteForm <$> areq (selectFieldList $ glist groups) "Group" Nothing

data BanForm = BanForm
  { banFormUsername :: Text
  , banFormIP       :: Maybe Text
  , banFormMessage  :: Maybe Text
  }

banForm :: Text -> Form BanForm
banForm username = renderDivs $
  BanForm
  <$> areq textField "Username" (Just username)
  <*> aopt textField "IP" Nothing
  <*> aopt textField "Message" Nothing

getUserAdminR :: Int64 -> Handler Html
getUserAdminR userid = do
  (uid, name, group) <- allowedToAdmin
  user'@(Entity uid' user) <- getUserById $ toSqlKey userid
  ad <- getGroup Administrator
  mo <- getGroup Moderator
  me <- getGroup Member
  (widp, enctp) <- generateFormPost $ promoteForm [ad, mo, me]
  (widb, enctb) <- generateFormPost $ banForm $ usersUsername $ entityVal user'
  profileLayout uid name group user' $(widgetFile "profile-info-promote")

postUserAdminR :: Int64 -> Handler Html
postUserAdminR userid = do
  (uid, name, group) <- allowedToAdmin
  promote <- lookupPostParam "promote"
  ban <- lookupPostParam "ban"
  case (promote, ban) of
    (Nothing, Nothing) -> invalidArgs ["Please make up your mind."]
    (Just _, Just _) -> invalidArgs ["Please make up your mind."]
    (Just _, Nothing) -> do
      ad <- getGroup Administrator
      mo <- getGroup Moderator
      me <- getGroup Member
      ((res, _), _) <- runFormPost $ promoteForm [ad, mo, me]
      case res of
        FormSuccess r -> do
          promoteUser
            uid
            name
            group
            (toSqlKey userid)
            (toSqlKey $ promoteFormGroupId r)
          redirect $ UserR userid
        _ -> invalidArgs ["Please fill the form correctly."]
    (Nothing, Just _) -> do
      ((res, _), _) <- runFormPost $ banForm ""
      case res of
        FormSuccess r -> do
          let (username, ip, message) =
                (banFormUsername r, banFormIP r, banFormMessage r)
          banUser uid name group (username) (ip) (message)
          redirect $ UserR userid
        _ -> invalidArgs ["Please fill the form correctly."]

getUserPostsR :: Int64 -> Handler Html
getUserPostsR userid = do
  (uid, name, group) <- allowedToPost
  user'@(Entity uid' user) <- getUserById $ toSqlKey userid
  posts <- getUserPosts $ toSqlKey userid
  profileLayout uid name group user' $(widgetFile "profile-info-posts")

getUserTopicsR :: Int64 -> Handler Html
getUserTopicsR userid = do
  (uid, name, group) <- allowedToPost
  user'@(Entity uid' user) <- getUserById $ toSqlKey userid
  topics <- getUserTopics $ toSqlKey userid
  profileLayout uid name group user' $(widgetFile "profile-info-topics")

