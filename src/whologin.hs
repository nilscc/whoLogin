{-# CFILES cbits/cleargreen.c cbits/minusred.c #-}
{-# LANGUAGE ScopedTypeVariables, PackageImports, ForeignFunctionInterface #-}

module Main
    (
    ) where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.Error
import Control.Concurrent
import Data.Maybe (listToMaybe, mapMaybe)
import Data.List (intercalate, isInfixOf, isPrefixOf)
import Foreign
import Graphics.UI.Gtk
import Network
import Text.JSON
import Data.Time
import System.Directory
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.FilePath
import System.IO
import System.Locale

import qualified Control.Exception as E
import qualified Graphics.UI.Gtk.Gdk.Events as Gdk


-- Some basic settings :)

appTitle, stuweHost, loginUrl, connStatusUrl :: String
appTitle                = "whologin 2.0"
stuweHost               = "stuwe.net.uni-tuebingen.de"
loginUrl                = "/netaccess/loginuser.html"
connStatusUrl           = "/netaccess/connstatus.html"

-- "Constants"
responseSave :: Int
responseSave = 0


-- Inline pixbufs :)
foreign import ccall "&cleargreen" clearGreen :: Ptr InlineImage
foreign import ccall "&minusred"   minusRed   :: Ptr InlineImage


--------------------------------------------------------------------------------
-- {{{ Main application
--------------------------------------------------------------------------------

data Flag = Verbose
          | NoGUI
          | ShowUsage
          | LoginOnce
          | LogFile FilePath
          deriving Eq

options :: [OptDescr Flag]
options =
    [ Option "o"        ["once"]        (NoArg LoginOnce)       "Perform login once"
    , Option "c"        ["cli"]         (NoArg NoGUI)           "Use CLI only. Don't load GUI"
    , Option "f"        ["file"]        (ReqArg LogFile "FILE") "Log to file"
    , Option "v"        ["verbose"]     (NoArg Verbose)
                        "Log to stdout. This is only possible in combination with --cli"
    , Option "h"        ["help"]        (NoArg ShowUsage)       "Show this help"
    ]

main :: IO ()
main = do

    -- Create MVar for our config
    cfgMVar <- loadConfig >>= newMVar

    -- Read arguments, ignore errors
    (opts, _, _) <- getOpt Permute options <$> getArgs

    -- Option flags
    let noGui   = NoGUI     `elem` opts
        verbose = Verbose   `elem` opts
        useage  = ShowUsage `elem` opts
        once    = LoginOnce `elem` opts
        logFile = listToMaybe $ mapMaybe getLogfile opts

        getLogfile (LogFile fp) = Just fp
        getLogfile _            = Nothing

    when useage $ do
        putStrLn $ usageInfo (appTitle ++ " - Usage:\n") options
        exitWith ExitSuccess

    when once $ do
        performLogin cfgMVar (Nothing :: Maybe StatusIcon)
        exitWith ExitSuccess

    if noGui
       then do
           modifyMVar_ cfgMVar $ \cfg -> return cfg
               { logToHandle = if verbose then Just stdout else Nothing
               , logBuffer   = Nothing          -- just to make sure
               , logToFile   = case logFile of
                                    Nothing -> logToFile cfg
                                    _       -> logFile
               }
           forkIO $ connectionLoop cfgMVar Nothing
           mainCLI cfgMVar

       else do
           -- Make sure we *never* output any data when the GUI is loaded
           modifyMVar_ cfgMVar $ \cfg -> return cfg
               { logToHandle = Nothing
               , logToFile   = case logFile of
                                    Nothing -> logToFile cfg
                                    _       -> logFile
               }
           forkIO . connectionLoop cfgMVar . Just =<< createGui cfgMVar
           mainGUI

-- | Run connection loop
connectionLoop :: MVar Config
               -> Maybe StatusIcon
               -> IO ()
connectionLoop cfgMVar icon = forever $ do
    performLogin cfgMVar icon
    cfg <- readMVar cfgMVar
    threadDelay (intervall cfg * 1000 * 1000)


-- | Create CLI
mainCLI :: MVar Config -> IO ()
mainCLI cfgMVar = do
    logger' "INIT: CLI loaded. Use ^C to quit"
    forever $ return ()
  where
    logger' s = readMVar cfgMVar >>= flip logger s

-- | Create main GUI
createGui :: MVar Config -> IO StatusIcon
createGui cfgMVar = do

    --
    -- Create GUI --------------------------------------------------------------
    --

    -- Load gtkrc
    rcAddDefaultFile "gtkrc"

    initGUI

    -- For threads
    timeoutAddFull (yield >> return True) priorityDefaultIdle 50

    -- Systray -----------------------------------------------------------------
    icon <- statusIconNewFromPixbuf =<< pixbufNewFromInline minusRed

    -- Load menu

    menuStatusIcon  <- menuNew

    -- Create menu items
    menuitemConnect     <- menuItemNewWithMnemonic "_Login"
    menuitemSettings    <- menuItemNewWithMnemonic "_Settings"
    menuitemLog         <- menuItemNewWithMnemonic "L_og"
    menuitemQuit        <- menuItemNewWithMnemonic "_Quit"

    mapM_ (\i -> menuShellAppend menuStatusIcon i >> widgetShowAll i)
          [ menuitemSettings
          , menuitemLog
          , menuitemConnect
          , menuitemQuit
          ]

    -- Settings windows --------------------------------------------------------

    settings <- dialogNew
    windowSetTitle settings $ appTitle ++ " - Settings"
    windowSetDefaultSize settings 450 250
    windowSetPosition settings WinPosCenter

    _ <- dialogAddButton settings "Save" ResponseOk
    _ <- dialogAddButton settings "Close" ResponseClose

    -- Add account settings
    entryAccountLogin       <- entryNew
    entryAccountPassword    <- entryNew
    entrySetVisibility entryAccountPassword False

    checkButtonArty5        <- checkButtonNewWithLabel "Pass Arty5 Website"

    -- Intervall settings
    spinButtonIntervall     <- spinButtonNewWithRange 0 50000 1

    -- Compose everything into a table
    tableAccount            <- tableNew 5 2 False

    labelNew (Just "Account Settings") >>= \l ->
        tableAttachDefaults tableAccount l 0 2 0 1

    labelNew (Just "Login name:") >>= \l -> do
        tableAttachDefaults tableAccount l 0 1 1 2
        tableAttachDefaults tableAccount entryAccountLogin 1 2 1 2

    labelNew (Just "Password:") >>= \l -> do
        tableAttachDefaults tableAccount l 0 1 2 3
        tableAttachDefaults tableAccount entryAccountPassword 1 2 2 3

    labelNew (Just "Intervall (seconds):") >>= \l -> do
        tableAttachDefaults tableAccount l 0 1 3 4
        tableAttachDefaults tableAccount spinButtonIntervall 1 2 3 4

    labelNew (Just "Other settings:") >>= \l -> do
        tableAttachDefaults tableAccount l 0 1 4 5
        tableAttachDefaults tableAccount checkButtonArty5 1 2 4 5

    dialogGetUpper settings >>= flip containerAdd tableAccount

    -- Show all children
    containerForall settings widgetShowAll

    -- Reload settings everytime we show our setting window again
    afterShow settings $ do
        cfg <- readMVar cfgMVar
        entrySetText entryAccountLogin (login cfg)
        entrySetText entryAccountPassword ""
        spinButtonSetValue spinButtonIntervall (fromIntegral $ intervall cfg)
        toggleButtonSetActive checkButtonArty5 (passArty5 cfg)
        widgetGrabFocus entryAccountLogin


    -- Response handler
    onResponse settings $ \res -> do
        case res of
             ResponseClose -> do
                widgetHide settings

             ResponseOk -> do

                -- Save login/password
                login       <- entryGetText entryAccountLogin
                password    <- entryGetText entryAccountPassword
                int         <- spinButtonGetValueAsInt spinButtonIntervall
                pass        <- toggleButtonGetActive checkButtonArty5

                -- Load & edit config
                cfg <- takeMVar cfgMVar

                let cfg'   = if null login || null password
                                then cfg
                                else cfg { login = login, password = password }

                    cfg''  = if 0 < int
                                then cfg' { intervall = int }
                                else cfg'

                    cfg''' = cfg'' { passArty5 = pass }

                saveConfig cfg'''
                putMVar cfgMVar cfg'''
                forkIO $ () <$ performLogin cfgMVar (Just icon)

                -- Close window again
                widgetHide settings

             _ -> return ()

    onKeyPress entryAccountLogin $ \e -> False <$
        case e of
             Gdk.Key { Gdk.eventKeyName = "Return" } -> widgetGrabFocus entryAccountPassword
             _                                       -> return ()

    onKeyPress entryAccountPassword $ \e -> False <$
        case e of
             Gdk.Key { Gdk.eventKeyName = "Return" } -> widgetGrabFocus spinButtonIntervall
             _                                       -> return ()

    onKeyPress spinButtonIntervall $ \e -> False <$
        case e of
             Gdk.Key { Gdk.eventKeyName = "Return" } -> widgetGrabFocus checkButtonArty5
             _                                       -> return ()

    onKeyPress checkButtonArty5 $ \e -> False <$
        case e of
             Gdk.Key { Gdk.eventKeyName = "Return" } -> dialogResponse settings ResponseOk
             _                                       -> return ()


    -- Log Window --------------------------------------------------------------

    dialogLog <- dialogNew
    windowSetTitle dialogLog (appTitle ++ " - Log")
    windowSetPosition dialogLog WinPosCenter
    windowSetDefaultSize dialogLog 450 250

    dialogAddButton dialogLog "Save to file" (ResponseUser responseSave)
    dialogAddButton dialogLog "Close"        (ResponseClose)

    bufferLog       <- textBufferNew Nothing
    textViewLog     <- textViewNewWithBuffer bufferLog

    textViewSetEditable textViewLog False

    -- Add buffer to our config
    modifyMVar_ cfgMVar $ \cfg -> return cfg { logBuffer = Just bufferLog }

    dialogGetUpper dialogLog >>= flip containerAdd textViewLog

    containerForall dialogLog widgetShowAll

    onResponse dialogLog $ \res -> do
        case res of
             ResponseUser u | u == responseSave -> do
                fdialog <- fileChooserDialogNew (Just "Save logfile")
                                                Nothing
                                                FileChooserActionSave
                                                [("Save", ResponseOk), ("Cancel", ResponseCancel)]

                widgetShowAll fdialog

                onResponse fdialog $ \re ->
                    case re of
                         ResponseOk -> do
                            fp      <- fileChooserGetFilename fdialog
                            startI  <- textBufferGetIterAtLine bufferLog 0
                            endI    <- textBufferGetIterAtLine bufferLog (-1)
                            log     <- textBufferGetText bufferLog startI endI False
                            case fp of
                                 Just fp' -> writeFile (if hasExtension fp' then fp' else fp' ++ ".txt") log
                                 _        -> widgetShowAll =<< messageDialogNew Nothing
                                                                                []
                                                                                MessageWarning
                                                                                ButtonsOk
                                                                                "No filename given."

                            widgetHide fdialog

                         _ -> widgetHide fdialog

                return ()

             ResponseClose -> widgetHide dialogLog
             _             -> return ()


    -- Systray Icon ------------------------------------------------------------

    statusIconSetTooltip icon appTitle

    onActivateLeaf menuitemQuit     $ mainQuit
    onActivateLeaf menuitemSettings $ do
        windowPresent settings
    onActivateLeaf menuitemLog      $ do
        windowPresent dialogLog
    onActivateLeaf menuitemConnect  $
        () <$ forkIO (do
            stat <- status <$> readMVar cfgMVar
            if stat == Connected
               then readMVar cfgMVar >>= \cfg -> logger cfg "ERROR: Already logged in"
               else performLogin cfgMVar (Just icon)
            )

    -- Add menu to icon
    on icon statusIconPopupMenu $ \maybeMouseButton timeStamp ->
        menuPopup menuStatusIcon $ case maybeMouseButton of
                                        Just mb -> Just (mb, timeStamp)
                                        Nothing -> Nothing

    on icon statusIconActivate $ do
        vis <- get settings widgetVisible
        if vis
           then widgetHide settings
           else windowPresent settings


    -- GUI initialised ---------------------------------------------------------

    readMVar cfgMVar >>= \cfg -> logger cfg "INIT: GUI loaded"
    return icon

-- }}}

--------------------------------------------------------------------------------
-- {{{ Connecting/disconnecting, updating status
--------------------------------------------------------------------------------

performLogin :: StatusIconClass icon
             => MVar Config
             -> Maybe icon
             -> IO ()
performLogin cfgMVar statusIcon = do

    cfg <- readMVar cfgMVar

    (err :: Either String (Maybe ConnectionStatus)) <- runErrorT $ do

        loggedIn <- liftIO isLoggedIn

        if loggedIn && status cfg == Connected
           then return Nothing
           else do
               -- Perform login, handle time outs
               waitM <- liftIO newEmptyMVar
               id <- liftIO . forkIO $ do
                   threadDelay (60 * 1000 * 1000)
                   putMVar waitM $ Left "Connection timed out"

               id' <- liftIO . forkIO $ do
                   r <- fmap Right (performLogin' cfg) `E.catch` \(e :: E.SomeException) ->
                       return $ Left (show e)
                   putMVar waitM r

               res <- liftIO $ takeMVar waitM
               liftIO $ mapM killThread [id, id']

               case res of
                   Right stat -> do

                       -- Update systray icon
                       case statusIcon of
                            Just icon -> liftIO . postGUIAsync $ do

                                let title = appTitle ++ " - " ++ showStatus stat
                                pixbuf <- pixbufNewFromInline $ if stat == Connected
                                                                   then clearGreen
                                                                   else minusRed

                                statusIconSetTooltip icon title
                                statusIconSetFromPixbuf icon pixbuf

                            _ -> return ()

                       -- Update status
                       liftIO $ modifyMVar_ cfgMVar $ \cfg -> return cfg { status = stat }
                       return $ Just stat

                   Left e -> throwError e

    case err of
         Left e         -> logger cfg ("ERROR: " ++ e)
         Right (Just s) -> logger cfg ("STATUS: " ++ showStatus s)
         _              -> return ()


performLogin' :: Config -> IO ConnectionStatus
performLogin' cfg@Config { login = user, password = pw } = do

    (err :: Either String ()) <- runErrorT $ do

       -- Avoid "session expired" message here:
       reply <- liftIO $ do
           httpGET  stuweHost loginUrl []
           httpPOST stuweHost loginUrl [] [ ("username", user)
                                          , ("password", pw)
                                          , ("Login", "Continue")
                                          ]

       case reply of
            s | isSuccess s -> liftIO $
                when (passArty5 cfg)
                     (checkArty5 >>= flip when (logger cfg "INFO: Passed Arty5 login page"))
              | hasFailed s -> throwError "Wrong username/password"
            _               -> throwError "Unknown error during login"


    case err of
         Left e   -> return $ DisconnectedWithError e
         Right () -> return Connected

isSuccess, hasFailed :: [String] -> Bool
isSuccess = or . map ("You are logged in." `isInfixOf`)
hasFailed = or . map ("Credentials Rejected" `isInfixOf`)

isLoggedIn :: IO Bool
isLoggedIn = isSuccess <$> httpGET stuweHost connStatusUrl []

-- Avoid the green "Arty5" website
checkArty5 :: IO Bool
checkArty5 = do
    g <- getLocation `fmap` httpGET "google.de" "/" [("Connection", "close")]
    case g of
         Just host -> True <$ httpPOST host "/auth/index.html/u" []
                                       [ ("user", "toster")
                                       , ("password", "toster")
                                       , ("cmd", "authenticate")
                                       ]
         _ -> return False

  where getLocation :: [String] -> Maybe String
        getLocation ("":_) = Nothing -- read header only
        getLocation (l:r)  | "Location: " `isPrefixOf` l &&
                             "/upload/custom/green/willkommen.html?" `isInfixOf` l = parseLocation l False
                           | otherwise = getLocation r
        getLocation _      = Nothing

        parseLocation :: String -> Bool -> Maybe String
        parseLocation [] _    = Nothing
        parseLocation l True  = Just $ takeWhile (/= '/') l
        parseLocation l False = case l of
                                     ('h':'t':'t':'p':':':'/':'/':r) -> parseLocation r True
                                     (_:r)                           -> parseLocation r False
                                     _                               -> Nothing


-- {{{ HTTP Requests -----------------------------------------------------------

printHeader :: (String,String) -> String
printHeader (k,v) = k ++ ": " ++ v

-- | Perform a simple HTTP GET request with HTTP/1.1 headers. Returns the
-- response as plain text
httpGET :: String               -- ^ Host
        -> String               -- ^ Path
        -> [(String,String)]    -- ^ Headers
        -> IO [String]          -- ^ Response
httpGET host path headers = withSocketsDo $ do
    h <- connectTo host (PortNumber 80)
    hSetBuffering h LineBuffering
    hPutStr h . (++ "\r\n\r\n") . intercalate "\r\n" $
        ("GET " ++ path ++ " HTTP/1.1") : map printHeader ( ("Host", host)
                                                          : ("User-Agent", "whologin (2010-06-16)")
                                                          : ("Accept", "*/*")
                                                          : headers
                                                          )
    lines `fmap` hGetContents h

-- | Perform a simple HTTP POST request with HTTP/1.1 headers. Returns the
-- response as plain text
httpPOST :: String              -- ^ Host
         -> String              -- ^ Path
         -> [(String,String)]   -- ^ Headers
         -> [(String,String)]   -- ^ Post fields
         -> IO [String]
httpPOST host path headers posts = withSocketsDo $ do
    h <- connectTo host (PortNumber 80)
    hPutStr h . intercalate "\r\n" $
        ( ("POST " ++ path ++ " HTTP/1.1")
        : map printHeader ( ("Host", host)
                          : ("User-Agent", "whologin (2010-06-16)")
                          : ("Content-Type", "application/x-www-form-urlencoded")
                          : ("Content-Length", show $ length postStr)
                          : headers
                          )
        ) ++ ["", postStr]
    hFlush h
    lines `fmap` hGetContents h

  where postStr = intercalate "&" $ map (\(k,v) -> k ++ "=" ++ v) posts

-- }}}

-- }}}

--------------------------------------------------------------------------------
--  {{{ Config loading/saving
--------------------------------------------------------------------------------

data Config = Config
    { login         :: String
    , password      :: String
    , intervall     :: Int
    , logToFile     :: Maybe FilePath
    , logBuffer     :: Maybe TextBuffer
    , logToHandle   :: Maybe Handle
    , passArty5     :: Bool
    , status        :: ConnectionStatus
    , unknown       :: [(String, JSValue)]
    }
    deriving Eq

data ConnectionStatus = Connected
                      | Disconnected
                      | DisconnectedWithError String
                      deriving Eq

showStatus :: ConnectionStatus -> String
showStatus Connected = "Connected"
showStatus Disconnected = "Disconnected"
showStatus (DisconnectedWithError e) = "Disconnected (" ++ e ++ ")"

data LogMethod = LogToFile FilePath
               | LogToHandle Handle
               deriving (Show, Eq)

logger :: Config -> String -> IO ()
logger Config { logBuffer = buff, logToFile = log, logToHandle = handle } s = do

    t <- timeStr
    let logStr = t ++ " - " ++ s ++ "\n"

    -- Log to GUI buffer
    case buff of
         Just buffer -> postGUIAsync $ do iter <- textBufferGetIterAtLine buffer (-1)
                                          textBufferInsert buffer iter logStr
         _           -> return ()

    -- Save to file
    case log of
         Just fp -> appendFile fp logStr
         _       -> return ()

    -- Log to handle
    case handle of
         Just h -> hPutStr h logStr
         _      -> return ()

timeStr :: IO String
timeStr =
    fmap (\t -> formatTime defaultTimeLocale "%F %T" t)
         getZonedTime

loadConfig :: IO Config
loadConfig = do

    -- Create directory if necessary
    dirpath   <- getAppUserDataDirectory "whologin"
    dirExists <- doesDirectoryExist dirpath
    unless dirExists $ createDirectory dirpath

    -- Filepaths
    let configFp  = dirpath ++ [pathSeparator] ++ "settings.json"

        nullConf  = Config
            { login         = ""
            , password      = ""
            , intervall     = 30
            , passArty5     = True
            , logToHandle   = Just stdout
            , logToFile     = Nothing
            , logBuffer     = Nothing
            , status        = Disconnected
            , unknown       = []
            }

    -- Load files if existing
    fileExists <- doesFileExist configFp
    if fileExists
       then jsonToConfig nullConf `fmap` readFile configFp
       else return nullConf

  where jsonToConfig cfg str = case decode str of
                                    Ok (JSObject o) -> foldr jsonToConfig' cfg (fromJSObject o)
                                    _               -> cfg

        jsonToConfig' ("login",JSString login)     cfg = cfg { login = fromJSString login }
        jsonToConfig' ("password",JSString pw)     cfg = cfg { password = fromJSString pw }
        jsonToConfig' ("intervall",JSRational _ i) cfg = cfg { intervall = round i }
        jsonToConfig' ("passArty5",JSBool b)       cfg = cfg { passArty5 = b }
        jsonToConfig' ("logToFile",JSString fp)    cfg = cfg { logToFile = Just $ fromJSString fp }
        jsonToConfig' unknown'                     cfg = cfg { unknown = unknown cfg ++ [unknown'] }


saveConfig :: Config -> IO ()
saveConfig cfg = do

    -- Create directory if necessary
    dirpath   <- getAppUserDataDirectory "whologin"
    dirExists <- doesDirectoryExist dirpath
    unless dirExists $ createDirectory dirpath

    let configFp  = dirpath ++ [pathSeparator] ++ "settings.json"

    writeFile configFp (encode $ configToJson cfg)

  where configToJson Config { login = l
                            , password = p
                            , intervall = i
                            , passArty5 = pass
                            , logToFile = log
                            , unknown = unknown'
                            } = toJSObject $ [ ("login", JSString $ toJSString l)
                                             , ("password", JSString $ toJSString p)
                                             , ("intervall", JSRational False (fromIntegral i))
                                             , ("passArty5", JSBool pass)
                                             ] ++ (maybe [] (\fp -> [("logToFile", JSString (toJSString fp))]) log)
                                               ++ unknown'

-- }}}
