{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Flux.AdmCategory where

import           Import

import           DBOp.CRUDCategory

createCategory Administrator catname =
  liftHandler $ runDB $ insertCategory catname
createCategory _ _ =
  permissionDenied "You're not allowed to do this (category creation)."

