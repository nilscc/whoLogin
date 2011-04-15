{-# LANGUAGE OverloadedStrings, NamedFieldPuns, PackageImports, TupleSections,
             TypeSynonymInstances, FlexibleInstances
             #-}

module Server where

import Control.Applicative
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Error
import Control.Concurrent.MVar
import Data.ByteString.UTF8
import Data.Maybe
import Snap.Http.Server
import Snap.Util.FileServe
import Snap.Types                       hiding (ifTop, route, getParam, method, getRequest)
import Text.Blaze
import Text.Blaze.Renderer.Utf8
import Text.JSONb                       as JS

import qualified Snap.Types             as S
import qualified Data.ByteString        as BS
import qualified Data.ByteString.UTF8   as B8
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Trie              as T

import Connect
import Config
import Templates.Index
import Types

getConfig :: Server Config
getConfig = asks stateConf >>= liftIO . readMVar

getLog :: Server Log
getLog = asks stateLog >>= liftIO . readMVar


--------------------------------------------------------------------------------
-- JSON functions

class IsJson a where
    toJson :: a -> JSON

-- {{{ Instances

instance IsJson JSON where
    toJson = id

instance IsJson BS.ByteString where
    toJson = JS.String

instance IsJson Bool where
    toJson = JS.Boolean

instance IsJson Rational where
    toJson = JS.Number

instance IsJson () where
    toJson = const JS.Null

instance IsJson [(BS.ByteString, JSON)] where
    toJson = JS.Object . T.fromList

instance IsJson Html where
    toJson = JS.String . BS.concat . LBS.toChunks . renderHtml

-- }}}

-- | "hack" for the OverloadedStrings extension
jsonStr :: BS.ByteString -> JSON
jsonStr = toJson

-- | "hack" for the OverloadedStrings extension
jsonObj :: [(BS.ByteString, JSON)] -> JSON
jsonObj = toJson

writeJson :: IsJson json => json -> Server ()
writeJson = lift . writeBS . JS.encode JS.Compact . toJson

writeHtml :: Html -> Server ()
writeHtml = lift . writeBS . BS.concat . LBS.toChunks . renderHtml

-- | Turn a "Html" data type into a JSON Object with @{ html: <html> }@
jsonHtml :: Html -> JSON
jsonHtml html = jsonObj [ ("html", toJson html) ]


--------------------------------------------------------------------------------
-- Snap functions

ifTop :: Server a -> Server a
ifTop srv = do
    s <- ask
    lift $ S.ifTop (runReaderT srv s)

route :: [(BS.ByteString, Server a)] -> Server a
route rs = do
    s <- ask
    lift $ S.route [ (l, runReaderT srv s) | (l, srv) <- rs ]

getParam :: BS.ByteString -> Server (Maybe ByteString)
getParam = lift . S.getParam

method :: Method -> Server a -> Server a
method m srv = do
    s <- ask
    lift $ S.method m $ runReaderT srv s

getRequest :: Server Request
getRequest = lift S.getRequest


--------------------------------------------------------------------------------
-- Main stuff

runServer :: State -> IO ()
runServer s = do
    Config { bind, port, access_log, error_log } <- readMVar (stateConf s)
    httpServe (fromString bind)
              port
              "localhost"
              (access_log)
              (error_log)
              (runReaderT server s)

server :: Server ()
server = do
    route [ (""                  , ifTop $ writeHtml documentT )
          , ("init"              , ajaxHandler initHandler     )
          , ("status"            , ajaxHandler loadStatus      )
          , ("settings"          , ajaxHandler loadSettings    )
          , ("settings/advanced" , ajaxHandler loadSettingsAdv )
          , ("settings/post"     , ajaxHandler postSettings    )
          ]
      <|> lift (fileServe ".")


--------------------------------------------------------------------------------
-- Snap Handler

initHandler :: Server JSON
initHandler = do
    Config { login } <- getConfig
    (ty, inner) <- if null login then
                       ("settings",) <$> loadSettings
                     else
                       ("status"  ,) <$> loadStatus

    let innerKeys = case inner of

            JS.Object trie -> [ (key, value)
                              | (key, def) <- [ ("html"      , jsonStr "")
                                              , ("connected" , toJson False) ]
                              , let value = fromMaybe def $ T.lookup key trie ]

            _              -> [ ("html", inner) ]

    return $ toJson ( ("type", JS.String ty)
                    : innerKeys
                    )

ajaxHandler :: Server JSON -> Server ()
ajaxHandler j = j >>= writeJson


--------------------------------------------------------------------------------
-- Load data

loadStatus, loadSettings, loadSettingsAdv :: Server JSON

loadSettings    = (jsonHtml . settingsT   ) <$> getConfig
loadSettingsAdv = (jsonHtml . settingsAdvT) <$> getConfig

loadStatus = do
    b <- isLoggedIn
    let cur | b         = "Connected"
            | otherwise = "Not connected"
    l <- getLog
    return $ jsonObj [ ("html",       toJson $ statusT cur l)
                     , ("connected",  toJson b)
                     ]


--------------------------------------------------------------------------------
-- Handle configuration

data SettingsError
    -- User & password
    = NoUser
    | NoPassword
    -- Time interval
    | IntervalTooBig
    | IntervalTooSmall
    | IntervalNaN
    -- Server settings
    | BindInvalid
    | PortTooBig
    | PortTooSmall
    | PortNaN
    -- Other
    | OtherError

instance Error SettingsError where
    noMsg = OtherError

postSettings :: Server JSON
postSettings = method POST $ do

    -- get possible all parameters
    un <- getParam "username"
    pw <- getParam "password"
    it <- getParam "interval"
    a5 <- getParam "arty5"
    bi <- getParam "bind"
    po <- getParam "port"

    cm  <- asks stateConf
    ers <- liftIO $ modifyMVar cm $
      \ c @ Config { login, password, interval, bind, port } ->

        -- Phew! This is a long block.
        -- Basicly, we check every parameter on errors, and return the errors
        -- on failure.
        --
        -- This is done in a "modifyMVar" block to assure thread safety.
        let (eu,u')  = splitErr $ do
                       u <- lift un
                       assert NoUser (not $ BS.null u && null login)
                       guard' . not $ BS.null u
                       return u

            (ep,p')  = splitErr $ do
                       p <- lift pw
                       assert NoPassword (not $ BS.null p && null password)
                       guard' . not $ BS.null p
                       return p
                       
            (ei,i')  = splitErr $ do
                       i <- lift it
                       case reads (B8.toString i) of
                            [(int,"")] -> do
                                assert IntervalTooSmall (int >= 5)
                                assert IntervalTooBig   (int <= 30 * 24 * 60 * 60)
                                return int
                            _ -> throwError IntervalNaN

            (eb,b')  = splitErr $ do
                       b <- lift bi
                       assert BindInvalid (not $ BS.null b)
                       -- not sure on what to check here?
                       return b

            (eo,o')  = splitErr $ do
                      p <- lift po
                      case reads (B8.toString p) of
                           [(i,"")] -> do
                               assert PortTooSmall (0 <= i)
                               assert PortTooBig   (i <= 65535)
                               return i
                           _ -> throwError PortNaN

            errors  = catMaybes [eu, ep, ei, eb, eo]

            c' | null errors = c { login     = maybe login    B8.toString u'
                                 , password  = maybe password B8.toString p'
                                 , interval  = fromMaybe interval i'
                                 , bind      = maybe bind     B8.toString b'
                                 , port      = fromMaybe port o'
                                 , passArty5 = isJust a5 -- bool, no need to validate
                                 }
               | otherwise = c

         in do unless (c' == c) (saveConfig c')
               return (c' , errors)

        -- end of "modifyMVar" block

    return $ jsonObj $
        if null ers then
            [ ("success", toJson True) ]
          else
            [ ("success", toJson False)
            , ("errors", jsonObj $ map errToId ers)
            ]
            

  where

    assert :: SettingsError -> Bool -> ErrorT SettingsError Maybe ()
    assert err b = if b then return () else throwError err

    -- A lifted version of guard for the Maybe monad inside the ErrorT:
    guard' :: Bool -> ErrorT SettingsError Maybe ()
    guard' True  = return ()
    guard' False = lift Nothing

    splitErr :: ErrorT SettingsError Maybe a -> (Maybe SettingsError, Maybe a)
    splitErr errt = case runErrorT errt of
                         Just (Left err) -> (Just err, Nothing)
                         Just (Right ok) -> (Nothing, Just ok)
                         _               -> (Nothing, Nothing)

    errToId :: SettingsError -> (BS.ByteString, JSON)
    errToId e = case e of
        NoUser           -> ("username-err", jsonStr "No username")
        NoPassword       -> ("password-err", jsonStr "No password")
        IntervalNaN      -> ("interval-err", jsonStr "Not a number")
        IntervalTooBig   -> ("interval-err", jsonStr "Interval too big")
        IntervalTooSmall -> ("interval-err", jsonStr "Interval too small")
        BindInvalid      -> ("bind-err",     jsonStr "Invalid address")
        PortNaN          -> ("port-err",     jsonStr "Not a number")
        PortTooBig       -> ("port-err",     jsonStr "Port too big")
        PortTooSmall     -> ("port-err",     jsonStr "Port too small")
        OtherError       -> ("other",        jsonStr "Please report this error.")
