{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Adm.Ban where

import           Import

import           Database.Esqueleto

import           Flux.Adm.Ban

data BanUserForm = BanUserForm
  { banUserFormUsername :: Text
  } deriving (Show)

data BanUserOptionsForm = BanUserOptionsForm
  { banUserOptionsFormUsername :: Text
  , banUserOptionsFormIp       ::Maybe Text
  , banUserOptionsFormMessage  ::Maybe Textarea
  }

banUserOptionsForm :: Text -> Form BanUserOptionsForm
banUserOptionsForm username =
  renderDivs $ BanUserOptionsForm
  <$> areq textField "Username" (Just username)
  <*> aopt textField "IP" Nothing
  <*> aopt textareaField "Message" Nothing

banUserForm :: Form BanUserForm
banUserForm = renderDivs $ BanUserForm <$> areq textField "Username" Nothing

getAdmBanR :: Handler Html
getAdmBanR = do
  (uid, name, group) <- allowedToMod
  (wid, enct) <- generateFormPost banUserForm
  bans <- getAllBanneds
  let banneds = map (\(Value uid, b, Value ename) -> (uid, b, ename)) bans
  adminLayout uid name group $(widgetFile "adm-ban")

postAdmBanR :: Handler Html
postAdmBanR = do
  (uid, name, group) <- allowedToMod
  names <- lookupPostParams "username"
  ban <- lookupPostParam "ban"
  case (names, ban) of
    ([], Nothing) -> invalidArgs ["Make up your mind!"]
    ([], Just _) -> do
      ((res, _), _) <- runFormPost $ banUserOptionsForm "x"
      case res of
        FormSuccess r -> do
          banUser
            uid
            name
            group
            (banUserOptionsFormUsername r)
            (banUserOptionsFormIp r)
            (unTextarea <$> banUserOptionsFormMessage r)
          redirect $ AdmBanR
        _ -> invalidArgs ["Your input is not correct."]
    (xs, Just _) -> invalidArgs ["Make up your mind!"]
    (xs, Nothing) -> do
      forM_ names $ unbanUser uid name group
      redirect AdmBanR

postAdmBanOptionsR :: Handler Html
postAdmBanOptionsR = do
  (uid, name, group) <- allowedToMod
  ((res, _), _) <- runFormPost banUserForm
  case res of
    FormSuccess r -> do
      (wid, enct) <-
        generateFormPost . banUserOptionsForm . banUserFormUsername $ r
      adminLayout uid name group $(widgetFile "adm-ban-options")
    _ -> invalidArgs ["Fill your input correctly."]

