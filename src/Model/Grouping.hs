{-# LANGUAGE TemplateHaskell #-}
module Model.Grouping where

import Database.Persist.TH

data Grouping
    = Administrator
    | Moderator
    | Member
    | Banned
    deriving (Show, Eq, Read, Ord)

derivePersistField "Grouping"
