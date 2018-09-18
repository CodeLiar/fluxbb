-- ApplicationSettings
module Settings where

import ClassyPrelude.Yesod
import Database.Persist.Postgresql
import Network.Wai.Handler.Warp

data ApplicationSettings = ApplicationSettings
    { appStaticDir              :: String
    , appRoot                   :: Maybe Text
    -- , appDatabaseConf           :: PostgresConf -- from "persistent-postgresql" package, module, Database.Persist.Postgresql
    , appHost                   :: HostPreference -- from "warp" package, module Network.Wai.Handler.Warp
    , appPort                   :: Int
    , appReloadTemplate         :: Bool
    , appMutableStatic          :: Bool
    , appSkipCombining          :: Bool
    , appDetailedRequestLogging :: Bool
    }
