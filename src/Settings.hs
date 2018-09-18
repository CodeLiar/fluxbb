-- ApplicationSettings
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
module Settings where

import ClassyPrelude.Yesod
import Control.Exception        as Exception
import Data.Aeson
import Data.FileEmbed
import Data.Yaml
import Database.Persist.Postgresql
import Language.Haskell.TH.Syntax
import Network.Wai.Handler.Warp
import Yesod.Default.Config2

data ApplicationSettings = ApplicationSettings
    { appStaticDir              :: String
    , appRoot                   :: Maybe Text
    , appDatabaseConf           :: PostgresConf -- from "persistent-postgresql" package, module, Database.Persist.Postgresql
    , appHost                   :: HostPreference -- from "warp" package, module Network.Wai.Handler.Warp
    , appPort                   :: Int
    , appReloadTemplate         :: Bool
    , appMutableStatic          :: Bool
    , appSkipCombining          :: Bool
    , appDetailedRequestLogging :: Bool
    }

instance FromJson ApplicationSettings where -- "aeson", module Data.Aeson
    parseJson =
        withObject "ApplicationSettings" $ \ob -> do
            let defEnv = True
            appStaticDir <- ob .: "static-dir"
            appRoot <- ob .:? "app-root"
            appHost <- fromString <$> ob .: "app-host"
            appDatabaseConf <- ob .: "database-conf"
            appProt <- ob .: "app-port"
            dev <- ob .: "development" .!= defEnv
            appReloadTemplate <- ob .:?  "reload-template" .!= dev
            appMutableStatic <- ob .:? "mutable-static" .!= dev
            appSkipCombining <- ob .:? "skip-combining" .!= dev
            appDetailedRequestLogging <- ob .:? "detailed-req-log" .!= dev
            return ApplicationSettings {..}

configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml) -- package "file-embed", module Data.Embed

configSettingsYmlValue :: Value
configSettingsYmlValue = eigher Exception.throw id $ decodeEither' configSettingsYmlBS -- package "yaml", module Data.Yaml
