{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Adm.Forum where

import           Import

import           Database.Esqueleto

import           Flux.AdmCategory
import           Flux.AdmForum

data CreateForumForm = CreateForumForm
  { createForumFormName     :: Text
  , createForumFormDesc     :: Maybe Textarea
  , createForumFormCategory :: Int64
  } deriving (Show)

createForumForm :: [Entity Categories] -> Form CreateForumForm
createForumForm cats = renderDivs $
  CreateForumForm
  <$> areq textField "Forum Name" Nothing
  <*> aopt textareaField "Description" Nothing
  <*> areq (selectFieldList catlist) "Category" Nothing
  where
    catlist =
      map (\(Entity cid (Categories name)) -> (name, fromSqlKey cid)) cats

getAdmForumR :: Handler Html
getAdmForumR = do
  (u, n, g) <- allowedToAdmin
  allcategories <- getAllCategories
  (wid, enct) <- generateFormPost $ createForumForm allcategories
  defaultLayout $ do
    [whamlet|
      <h3> Create Forum
      <form method=post action=@{AdmForumR} enctype=#{enct}>
        ^{wid}
        <input .button-primary name=create value=create type=submit>
    |]

postAdmForumR :: Handler Html
postAdmForumR = do
  (u, n, g) <- allowedToAdmin
  allcategories <- getAllCategories
  ((res, _), _) <- runFormPost $ createForumForm allcategories
  case res of
    FormFailure x -> invalidArgs x
    FormSuccess r -> do
      createForum
        g
        (toSqlKey $ createForumFormCategory r)
        (createForumFormName r)
        (unTextarea <$> createForumFormDesc r)
      redirect AdmForumR
    _ -> invalidArgs ["Good job, smarty pants!"]

