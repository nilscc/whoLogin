{-# LANGUAGE RankNTypes, ViewPatterns, PackageImports #-}

module Connect ( isLoggedIn, performLogin ) where

import "mtl" Control.Monad.Trans
import "mtl" Control.Monad.Error
import Data.List
import Data.Maybe
import Network.HTTP
import Network.URI
import Network.Browser

--------------------------------------------------------------------------------
-- Settings

stuweHost, loginUrl, connStatusUrl, arty5, arty5Login :: String

stuweHost     = "http://stuwe.net.uni-tuebingen.de"
loginUrl      = "/netaccess/loginuser.html"
connStatusUrl = "/netaccess/connstatus.html"

arty5         = "/upload/custom/green/willkommen.html?"
arty5Login    = "/auth/index.html/u"

loginForm :: String -> String -> [(String,String)]
loginForm u p = [ ("username" , u)
                , ("password" , p)
                , ("Login"    , "Continue")
                ]

arty5Form :: [(String,String)]
arty5Form     = [ ("user"     , "toster")
                , ("password" , "ninja")
                , ("cmd"      , "authenticate")
                ]

--------------------------------------------------------------------------------
-- Helpers

io :: MonadIO m => IO a -> m a
io = liftIO

-- Some sane defaults
doBrowse :: MonadIO m => BrowserAction conn a -> m a
doBrowse baction = io . browse $ do
    setAllowRedirects False
    setErrHandler $ \_ -> return ()
    setOutHandler $ \_ -> return ()
    baction

toUri :: String -> URI
toUri = fromMaybe nullURI . parseURI

get :: MonadIO m => String -> m (Response String)
get s = get' . toUri $ stuweHost ++ s

get' :: MonadIO m => URI -> m (Response String)
get' u = do
    (_,r) <- doBrowse $ request $ mkRequest GET u
    return r

post :: MonadIO m => String -> [(String,String)] -> m (Response String)
post s = post' . toUri $ stuweHost ++ s

post' :: MonadIO m => URI -> [(String,String)] -> m (Response String)
post' u l = do
    (_,r) <- doBrowse $ request $ formToRequest $ Form POST u l
    return r

--------------------------------------------------------------------------------
-- Types

type Username = String
type Password = String

--------------------------------------------------------------------------------
-- Actions

isSuccess, hasFailed :: (Response String) -> Bool
isSuccess (rspBody -> r) = "You are logged in." `isInfixOf` r
hasFailed (rspBody -> r) = "Credentials Rejected" `isInfixOf` r

isLoggedIn :: MonadIO m => m Bool
isLoggedIn = do
    r <- get connStatusUrl
    return $ isSuccess r


-- | Check if we have to pass the green Arty5 page, returns for the html-form
-- if necessary
checkArty5 :: MonadIO m => m (Maybe URI)
checkArty5 = do
    g <- get' $ toUri "http://google.de"
    case findHeader HdrLocation g of
         Just h | arty5 `isInfixOf` h -> return $
            let Just uri = parseURI $ takeWhile ('?' /=) h -- hacky :)
             in Just uri { uriPath = arty5Login
                         , uriQuery = ""
                         , uriFragment = ""
                         }
         _ -> return Nothing


passArty5 :: MonadIO m => URI -> m Bool
passArty5 uri = do
    _ <- post' uri arty5Form
    checkArty5 >>= return . isNothing


performLogin :: (Functor m, MonadIO m)
             => Username
             -> Password
             -> Bool                    -- ^ pass arty5 page?
             -> m (Either String (Maybe String))
performLogin un pw a5 = runErrorT $ do
    l <- isLoggedIn
    if l then
        return Nothing
      else do
        _ <- get loginUrl
        r <- post loginUrl (loginForm un pw)
        case r of
             s | isSuccess s && a5 -> do
                 b <- checkArty5
                 case b of
                      Just uri -> do
                          success <- passArty5 uri
                          if success
                             then return $ Just "Login successful, passed ARTY5 page."
                             else throwError    "Login successful but failed to pass ARTY5 page."
                      _ -> return $ Just        "Login successful."
               | isSuccess s -> return $ Just   "Login successful."
               | hasFailed s -> throwError      "Wrong username/password."
               | otherwise   -> throwError $    "Unknown error during login, reply from the server was: " ++ (show s)
