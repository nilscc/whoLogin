{-# LANGUAGE PackageImports #-}

module Types
    ( State (..)
    , Server
    , Config (..)
    , Log
    ) where

import Control.Concurrent.MVar
import "mtl" Control.Monad.Reader
import Data.Time.LocalTime
import Snap.Types
import Text.JSON

data Config = Config
    { login         :: String
    , password      :: String
    , passArty5     :: Bool
    , interval      :: Int
    , bind          :: String
    , port          :: Int
    , status_log    :: Maybe FilePath
    , access_log    :: Maybe FilePath
    , error_log     :: Maybe FilePath
    , unknown       :: [(String, JSValue)]
    }
    deriving (Show, Eq)

type Log            = [(ZonedTime, String)]

data State = State
    { stateLog  :: MVar Log
    , stateConf :: MVar Config
    }

type Server a = ReaderT State Snap a
