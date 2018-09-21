{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell       #-}
module Handler.Adm.Forum where

import           Import

import           Database.Esqueleto

import           Flux.Adm.Category
import           Flux.Adm.Forum

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
  catfnamekeys <- getForumsAndItsCategory
  (wid, enct) <- generateFormPost $ createForumForm allcategories
  adminLayout u n g $ do
    $(widgetFile "adm-forum")

postAdmForumR :: Handler Html
postAdmForumR = do
  (u, n, g) <- allowedToAdmin
  allcategories <- getAllCategories
  createparam <- lookupPostParam "create"
  deleteparam <- lookupPostParam "delete"
  case (createparam, deleteparam) of
    (Just _, Nothing) -> do
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
    (Nothing, Just _) -> do
      deletions <- lookupPostParams "delete-forum-id"
      deleteForums g deletions
      redirect AdmForumR
    _ -> invalidArgs ["What do you want? Create or delete?"]

