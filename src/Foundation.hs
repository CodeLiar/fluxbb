{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Foundation where

import           Import.NoFoundation hiding ((&&.), (==.))

import           Database.Esqueleto
import           Text.Hamlet
import           Text.Jasmine

import           Yesod.Auth.HashDB
import           Yesod.Auth.Message
import           Yesod.Core.Types
import           Yesod.Default.Util
import           Yesod.Form
import           Yesod.Static

data App = App
  { appSettings       :: ApplicationSettings
  , appConnectionPool :: ConnectionPool
  , appLogger         :: Logger
  , appStatic         :: Static
  , appHttpManager    :: Manager
  }

mkYesodData
  "App"
  [parseRoutes|
    /                        HomeR               GET
    /static                  StaticR             Static appStatic
    /auth                    SigninR             Auth getAuth
    /register                RegisterR           GET POST
    /profile                 ProfileR            GET
    /user/#Int64             UserR               GET
    /user/#Int64/edit        UserEditR           GET POST
    /user/#Int64/admin       UserAdminR          GET POST
    /admin                   AdmR                GET
    /admin/category          AdmCategoryR        GET POST
    /admin/forum             AdmForumR           GET POST
    /admin/ban               AdmBanR             GET POST
    /admin/ban/options       AdmBanOptionsR      POST
    /admin/report            AdmReportR          GET POST
    /admin/user              AdmUserR            GET POST
    /admin/user/promote      AdmUserPromoteR     POST
    /admin/user/promote/exe  AdmUserPromoteExeR  POST
    /forum/#Int64            ForumR              GET POST
    /forum/#Int64/#Int64     ForumPageR          GET
    /topic/#Int64            TopicR              GET POST
    /topic/#Int64/#Int64     TopicPageR          GET
    /post/#Int64             PostR               GET
    /post/#Int64/edit        PostEditR           GET POST
    /post/#Int64/report      PostReportR         GET POST
    /userlist                UserListR           GET POST
  |]

type Form a = Html -> MForm (HandlerFor App) (FormResult a, Widget)

type DB a = forall (m :: * -> *). (MonadIO m) => ReaderT SqlBackend m a

instance Yesod App where
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing   -> getApprootText guessApproot app req
      Just root -> root
  makeSessionBackend _ = Just <$> defaultClientSessionBackend (60 * 5) "config/client-session-key.aes"
  yesodMiddleware = defaultYesodMiddleware
  addStaticContent ext mime content = do
    yes <- getYesod
    let statdir = appStaticDir $ appSettings yes
    addStaticContentExternal
      minifym
      genFilename
      statdir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      genFilename lbs = "autogen-" ++ base64md5 lbs
  defaultLayout widget = do
    master <- getYesod
    muidnamegroup <- getUserAndGrouping
    mmessage <- getMessage
    pagecontent <- widgetToPageContent $ do
      addStylesheet $ StaticR css_main_css
      addStylesheet $ StaticR css_milligram_min_css
      addStylesheet $ StaticR css_main_css
      $(widgetFile "def")
    withUrlRenderer $(hamletFile "templates/wrapper.hamlet")
  authRoute _ = Just $ SigninR LoginR
  isAuthorized (SigninR _) _ = return Authorized
  isAuthorized HomeR _       = return Authorized
  isAuthorized RegisterR _   = return Authorized
  isAuthorized (StaticR _) _ = return Authorized
  isAuthorized _ _           = isLoggedIn

isLoggedIn :: Handler AuthResult
isLoggedIn = do
  maut <- maybeAuth
  case maut of
    Nothing -> return $ Unauthorized "login please"
    Just _  -> return Authorized

isNotLoggedIn :: Handler ()
isNotLoggedIn = do
  maut <- maybeAuth
  case maut of
    Nothing -> return ()
    Just _  -> redirect $ HomeR

getUserAndGrouping :: Handler (Maybe (Key Users, Text, Grouping))
getUserAndGrouping = do
  maut <- maybeAuth
  case maut of
    Nothing -> return Nothing
    Just (Entity uid user) -> do
      [gro] <-
        liftHandler $
        runDB $
        select $
        from $ \(group, user) -> do
          where_
            (user ^. UsersId ==. val uid
             &&. group ^. GroupsId ==. user ^. UsersGroupId)
          limit 1
          return (group ^. GroupsGrouping)
      return $ Just (uid, usersUsername user, unValue gro)

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnectionPool master

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodAuth App where
  type AuthId App = UsersId
  loginDest _ = HomeR
  logoutDest _ = HomeR
  redirectToReferer _ = False
  authPlugins _ = [authHashDBWithForm loginform (Just . UniqueUsername)]
    where
      loginform :: Route App -> Widget
      loginform action = $(whamletFile "templates/login.hamlet")
  authenticate creds = liftHandler $ runDB $ do
    x <- getBy $ UniqueUsername $ credsIdent creds
    case x of
      Nothing             -> return $ UserError InvalidLogin
      Just (Entity uid _) -> return $ Authenticated uid

instance YesodAuthPersist App

allowedToPost :: Handler (Key Users, Text, Grouping)
allowedToPost = do
  midnamegroup <- getUserAndGrouping
  case midnamegroup of
    Nothing -> permissionDenied "You're not allowed to see this page."
    (Just (uid, name, Banned)) -> permissionDenied "You're banned."
    (Just (uid, name, group)) -> return (uid, name, group)

allowedToMod :: Handler (Key Users, Text, Grouping)
allowedToMod = do
  (uid, name, group) <- allowedToPost
  case group of
    x | x == Administrator || x == Moderator -> return (uid, name, group)
    otherwise -> permissionDenied "You are not allowed to moderate this site."

allowedToAdmin :: Handler (Key Users, Text, Grouping)
allowedToAdmin = do
  (uid, name, group) <- allowedToPost
  case group of
    Administrator -> return (uid, name, group)
    _ -> permissionDenied "You are not allowed to administer this site."

adminLayout :: Key Users -> Text -> Grouping -> Widget -> Handler Html
adminLayout uid name group widget = do
  mcurrentroute <- getCurrentRoute
  mmessage <- getMessage
  pagecontent <- widgetToPageContent $ do
    addStylesheet $ StaticR css_normalize_css
    addStylesheet $ StaticR css_milligram_min_css
    addStylesheet $ StaticR css_main_css
    $(widgetFile "adm")
  withUrlRenderer $(hamletFile "templates/wrapper.hamlet")

adminRouteToText :: Route App -> Text
adminRouteToText AdmR           = "Index"
adminRouteToText AdmBanR        = "Manage Ban"
adminRouteToText AdmBanOptionsR = "Ban Options"
adminRouteToText AdmCategoryR   = "Manage Categories"
adminRouteToText AdmForumR      = "Manage Forums"
adminRouteToText AdmUserR       = "Manage User"
adminRouteToText _              = "Fix Me"


profileLayout ::
     Key Users -> Text -> Grouping -> Entity Users -> Widget -> Handler Html
profileLayout uid name group user widget = do
  let allowededit = allowedToEditProfile uid group (entityKey user)
  mcurrentroute <- getCurrentRoute
  mmessage <- getMessage
  pagecontent <- widgetToPageContent $ do
    addStylesheet $ StaticR css_normalize_css
    addStylesheet $ StaticR css_milligram_min_css
    addStylesheet $ StaticR css_main_css
    $(widgetFile "profile")
  withUrlRenderer $(hamletFile "templates/wrapper.hamlet")

allowedToEditProfile uid group profileid =
  profileid == uid || group == Administrator

profileRouteToText :: Route App -> Text
profileRouteToText ProfileR       = "Common Information"
profileRouteToText (UserR _)      = "Common Information"
profileRouteToText (UserEditR _)  = "Edit Common Information"
profileRouteToText (UserAdminR _) = "Promote User"
profileRouteToText _              = "Not Needed"


data SortBy
  = Username
  | Registered
  | PostCount
  deriving (Eq, Enum, Bounded)

instance Show SortBy where
  show Username   = "Username"
  show Registered = "Registration Date"
  show PostCount  = "Post Count"

