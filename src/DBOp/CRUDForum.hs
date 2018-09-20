{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module DBOp.CRUDForum where

import           Import hiding ((==.))

import Database.Esqueleto

insertForum cid name desc = do
  insert_ $ Forums cid name desc 0 0 Nothing Nothing Nothing

