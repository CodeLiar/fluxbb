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
  catfnamekeys <- getForumsAndItsCategory
  (wid, enct) <- generateFormPost $ createForumForm allcategories
  defaultLayout $ do
    [whamlet|
      <h3> Create Forum
      <form method=post action=@{AdmForumR} enctype=#{enct}>
        ^{wid}
        <input .button-primary name=create value=create type=submit>
            <h3> Delete Forums
      <form method=post action=@{AdmForumR} enctype=#{enct}>
        $forall (catname, fnamekeys) <- catfnamekeys
          <h4> Category: #{catname}
          <table>
            <thead>
              <th width="70%"> Name
              <th> Delete
            <tbody>
              $forall (name, key) <- fnamekeys
                <tr>
                  <td> #{name}
                  <td> <input name=delete-forum-id value=#{fromSqlKey key} type=checkbox>
        <input .button-primary name=delete value=delete type=submit>
    |]

postAdmForumR :: Handler Html
postAdmForumR = do
  (u, n, g) <- allowedToAdmin
  allcategories <- getAllCategories
  createparam <- lookupPostParam "create"
  deleteparam <- lookupPostParam "delete"
  case (createparam, deleteparam) of
    (Nothing, Nothing) -> invalidArgs ["What do you want? Create or delete?"]
    (Just _, Just _) -> invalidArgs ["What do you want? Create or delete?"]
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

