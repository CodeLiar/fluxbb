{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Flux.Adm.Report where

import           Import             hiding (Value)

import           Database.Esqueleto

import           DBOp.CRUDReport

getAllZappedReports ::
     ( BackendCompatible SqlBackend (YesodPersistBackend site)
     , PersistQueryRead (YesodPersistBackend site)
     , PersistUniqueRead (YesodPersistBackend site)
     , YesodPersist site
     )
  => HandlerFor site [(Entity Reports, Text, Text, Text, Text)]
getAllZappedReports = do
  reports <- liftHandler $ runDB $ selectReportsZappedInfo
  return $
    map (\(x, Value a, Value b, Value c, Value d) -> (x, a, b, c, d)) reports

getAllUnzappedReports ::
     ( BackendCompatible SqlBackend (YesodPersistBackend site)
     , PersistQueryRead (YesodPersistBackend site)
     , PersistUniqueRead (YesodPersistBackend site)
     , YesodPersist site
     )
  => HandlerFor site [(Entity Reports, Text, Text, Text)]
getAllUnzappedReports = do
  reports <- liftHandler $ runDB $ selectReportsUnzappedInfo
  return $ map (\(x, Value a, Value b, Value c) -> (x, a, b, c)) reports

readReports ::
     ( YesodPersistBackend (HandlerSite m) ~ SqlBackend
     , YesodPersist (HandlerSite m)
     , MonadHandler m
     )
  => Key Users
  -> [Text]
  -> m ()
readReports uid reportids = do
  liftHandler $
    runDB $ do
      let x = map readMay reportids :: [Maybe Int64]
          y = map (toSqlKey <$>) x :: [(Maybe (Key Reports))]
      forM_ y $ \z -> do
        case z of
          Nothing -> return ()
          Just x -> updateReportZap uid x

