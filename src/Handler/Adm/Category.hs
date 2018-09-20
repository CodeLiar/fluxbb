{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Adm.Category where

import           Import

import           Database.Esqueleto

import           Flux.AdmCategory

data CreateCategoryForm = CreateCategoryForm
  { createCategoryFormName :: Text
  } deriving (Show)

createCategoryForm :: Form CreateCategoryForm
createCategoryForm =
  renderDivs $ CreateCategoryForm <$> areq textField "Category Name" Nothing

data SelectCategoryForm = SelectCategoryForm
  { selectCategoryFormId :: Int64
  } deriving (Show)

selectCategoryForm :: [Entity Categories] -> Form SelectCategoryForm
selectCategoryForm cats =
  renderDivs $
  SelectCategoryForm <$> areq (selectFieldList catlist) "Category" Nothing
  where
    catlist :: [(Text, Int64)]
    catlist =
      map (\(Entity cid (Categories name)) -> (name, fromSqlKey $ cid)) cats

allowedToAdmin :: Handler (Key Users, Text, Grouping)
allowedToAdmin = do
  midnamegroup <- getUserAndGrouping
  case midnamegroup of
    Nothing -> permissionDenied "You're not allowed to see this page."
    (Just (uid, name, Administrator)) -> return (uid, name, Administrator)
    (Just (uid, name, _)) -> permissionDenied "You're not the admin of this site."

getAdmCategoryR :: Handler Html
getAdmCategoryR = do
  (uid, name, group) <- allowedToAdmin
  (widc, enctc) <- generateFormPost createCategoryForm
  allcategories <- getAllCategories
  (widl, enctl) <- generateFormPost $ selectCategoryForm allcategories
  defaultLayout
    [whamlet|
      <form method=post enctype=#{enctc}>
        ^{widc}
        <input .button-primary value=create type=submit>
    <hr>
      <form method=post enctype=#{enctl}>
        ^{widl}
        <input .button-primary value=delete type=submit>
    |]

postAdmCategoryR :: Handler Html
postAdmCategoryR = do
  (uid, name, group) <- allowedToAdmin
  ((res, _), _) <- runFormPost createCategoryForm
  case res of
    FormFailure x -> invalidArgs x
    FormSuccess r -> do
      _ <- createCategory group (createCategoryFormName r)
      redirect AdmCategoryR
    _ -> invalidArgs ["Good job, smarty pants!"]


