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

getAdmCategoryR :: Handler Html
getAdmCategoryR = do
  (widc, enctc) <- generateFormPost createCategoryForm
  defaultLayout
    [whamlet|
      <form enctype=#{enctc}>
        ^{widc}
        <input .button-primary value=create type=submit>
    |]

