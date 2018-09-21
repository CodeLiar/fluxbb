{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module DBOp.CRUDGroup where

import           Import                        hiding (Value, groupBy, on,
                                                update, (+=.), (=.), (==.))

import           Database.Esqueleto

selectGroupByGrouping groupname = do
  select $
    from $ \group -> do
      where_ (group ^. GroupsGrouping ==. val groupname)
      limit 1
      return group

