{-# LANGUAGE NamedFieldPuns, PackageImports #-}

module Config
    ( Config (..)
    , loadConfig
    , saveConfig
    ) where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.Trans
import Text.JSON
import System.Directory
import System.FilePath

import Types

-- | Load the \"settings.json\" file from the \"whologin\" appdata directory.
loadConfig :: MonadIO m => m Config
loadConfig = liftIO $ do

    -- Create directory if necessary
    dirpath   <- getAppUserDataDirectory "whologin"
    dirExists <- doesDirectoryExist dirpath
    unless dirExists $ createDirectory dirpath

    -- Filepaths
    let configFp  = dirpath ++ [pathSeparator] ++ "settings.json"

        nullConf  = Config
            { login      = ""
            , password   = ""
            , interval   = 30
            , bind       = "127.0.0.1"
            , port       = 8282
            , passArty5  = False
            , status_log = Just "status.log"
            , access_log = Just "access.log"
            , error_log  = Just "error.log"
            , unknown    = []
            }

    -- Load files if existing
    fileExists <- doesFileExist configFp
    if fileExists
       then jsonToConfig nullConf <$> readFile configFp
       else return nullConf

  where jsonToConfig cfg str = case decode str of
                                    Ok (JSObject o) -> foldr jsonToConfig' cfg (fromJSObject o)
                                    _               -> cfg

        jsonToConfig' ("login",JSString login')    cfg = cfg { login = fromJSString login' }
        jsonToConfig' ("password",JSString pw)     cfg = cfg { password = fromJSString pw }
        jsonToConfig' ("interval",JSRational _ i)  cfg = cfg { interval = round i }
        jsonToConfig' ("passArty5",JSBool b)       cfg = cfg { passArty5 = b }
        jsonToConfig' ("status_log",JSString fp)   cfg = cfg { status_log = Just $ fromJSString fp }
        jsonToConfig' ("access_log",JSString fp)   cfg = cfg { access_log = Just $ fromJSString fp }
        jsonToConfig' ("error_log",JSString fp)    cfg = cfg { error_log = Just $ fromJSString fp }
        jsonToConfig' ("bind",JSString b)          cfg = cfg { bind = fromJSString b }
        jsonToConfig' ("port",JSRational _ p)      cfg = cfg { port = round p }
        jsonToConfig' unknown'                     cfg = cfg { unknown = unknown cfg ++ [unknown'] }


saveConfig :: MonadIO m => Config -> m ()
saveConfig cfg = liftIO $ do

    -- Create directory if necessary
    dirpath   <- getAppUserDataDirectory "whologin"
    dirExists <- doesDirectoryExist dirpath
    unless dirExists $ createDirectory dirpath

    let configFp  = dirpath ++ [pathSeparator] ++ "settings.json"

    writeFile configFp (encode $ configToJson cfg)

  where configToJson Config { login
                            , password
                            , interval
                            , passArty5
                            , bind
                            , port
                            , status_log
                            , access_log
                            , error_log
                            , unknown
                            } = toJSObject $ [ ("login", JSString $ toJSString login)
                                             , ("password", JSString $ toJSString password)
                                             , ("interval", JSRational False (fromIntegral interval))
                                             , ("passArty5", JSBool passArty5)
                                             , ("bind", JSString $ toJSString bind)
                                             , ("port", JSRational False (fromIntegral port))
                                             ] ++ (maybe [] (\fp -> [("status_log", JSString (toJSString fp))]) status_log)
                                               ++ (maybe [] (\fp -> [("access_log", JSString (toJSString fp))]) access_log)
                                               ++ (maybe [] (\fp -> [("error_log", JSString (toJSString fp))]) error_log)
                                               ++ unknown
