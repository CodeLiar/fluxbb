{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell       #-}
module Handler.Adm.Category where

import           Import

import           Database.Esqueleto

import           Flux.Adm.Category

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
  (uid, name, group) <- allowedToAdmin
  (widc, enctc) <- generateFormPost createCategoryForm
  allcategories <- getAllCategories
  (widl, enctl) <- generateFormPost $ selectCategoryForm allcategories
  adminLayout uid name group $ do
    setTitle "Category Administration"
    $(widgetFile "adm-category")


postAdmCategoryR :: Handler Html
postAdmCategoryR = do
  (uid, name, group) <- allowedToAdmin
  createparam <- lookupPostParam "create"
  deleteparam <- lookupPostParam "delete"
  case (createparam, deleteparam) of
    (Nothing, Nothing) -> invalidArgs ["At least be sure of what you want."]
    (Just _, Nothing) -> do
      ((res, _), _) <- runFormPost createCategoryForm
      case res of
        FormFailure x -> invalidArgs x
        FormSuccess r -> do
          _ <- createCategory group (createCategoryFormName r)
          redirect AdmCategoryR
        _ -> invalidArgs ["Good job, smarty pants!"]
    (Nothing, Just _) -> do
      allcategories <- getAllCategories
      ((res, _), _) <- runFormPost $ selectCategoryForm allcategories
      case res of
        FormFailure x -> invalidArgs x
        FormSuccess r -> do
          deleteCategoryCascade group $ toSqlKey $ selectCategoryFormId r
          redirect AdmCategoryR
        _ -> invalidArgs ["Good job, smarty pants!"]
    (Just _, Just _) -> invalidArgs ["Make up your mind, my dear admin."]

