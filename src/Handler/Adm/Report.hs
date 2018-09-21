{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Adm.Report where

import           Import

import           Data.Time
import           Database.Esqueleto

import           Flux.Adm.Report

getAdmReportR :: Handler Html
getAdmReportR = do
  (uid, name, group) <- allowedToMod
  unzappedreports <- getAllUnzappedReports
  zappedreports <- getAllZappedReports
  adminLayout uid name group $ do
    setTitle "Reports Management"
    $(widgetFile "adm-report")

postAdmReportR :: Handler Html
postAdmReportR = do
  (uid, name, group) <- allowedToMod
  reportids <- lookupPostParams "report-id"
  readReports uid reportids
  redirect AdmReportR

