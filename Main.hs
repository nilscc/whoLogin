module Main where

import WhoAuth
import Config (setConf, loadConf, (##))
import Logfile (logToFile)

import GHC.Conc (threadDelay)

import Control.Monad (liftM2, unless)
import Control.Applicative ((<$))

import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Data.Time.LocalTime
import Data.Time.Calendar (showGregorian)

import System.Environment (getArgs)
import System.Console.GetOpt

import System.IO        (hFlush, stdout)
import System.FilePath  (pathSeparator)
import System.Directory ( getAppUserDataDirectory
                        , doesFileExist
                        , doesDirectoryExist
                        , createDirectory
                        )

-- {{{ Data definitions

data Options = Options  { pName     :: String
                        , pVersion  :: Double

                        , name      :: Maybe String
                        , pw        :: Maybe String

                        , runLevel  :: RunLevel
                        , config    :: Maybe FilePath
                        , logFile   :: Maybe FilePath
                        } deriving Show

data RunLevel = Once | Background Int | Configuration deriving Show

-- }}}

-- | Start whoLogin,
-- Show info, then load the config file, but prefer arguments.
main = do
    args <- getArgs

    logPath     <- pathToLog
    confPath    <- pathToConf

    -- Load Options from Configfile and replace with arguments
    opts'       <- loadConfig confPath defaultOptions
    let opts    =  parseArgs options args opts'


    showInfo

    -- only for debugging:
    -- putStrLn $ show opts

    -- TODO:
    -- startLog logPath

    runWithConfig opts

    -- endLog logPath

-- {{{ Settings

-- | Default options
defaultOptions = Options    { pName     = "whoLogin"
                            , pVersion  = 0.12

                            , name      = Nothing
                            , pw        = Nothing

                            , runLevel  = Background 60
                            , config    = Nothing
                            , logFile   = Nothing
                            }

-- | Define some options for the CLI
options =   [ let runOnce o = o { runLevel = Once }
              in  Option "o"    ["once"]        (NoArg runOnce) "Nur einmal ausführen"

            , let runBG i o = o { runLevel = Background . read $ i }
              in  Option "b"    ["background"]  (ReqArg runBG "SECONDS") "Wiederholt ausführen, Interval in Sekunden"

            , let runConfiguration o = o { runLevel = Configuration }
              in  Option "c"    ["config"]      (NoArg runConfiguration) "Konfigurationsdatei erstellen"

            -- TODO:
            --, let runWithConfig fp o = o { config = Just fp }
            --  in  Option ""     ["loadConfig"]  (ReqArg runWithConfig "FILE") "Eigene Konfigurationsdatei verwenden"
            --, let runOwnLog fp o = o { logFile = Just fp }
            --  in  Option ""     ["logfile"]  (ReqArg runOwnLog "FILE") "Eigene Logdatei verwenden"
            ]


-- | Default filepath for configuration
pathToConf :: IO FilePath
pathToConf = do
    dirpath <- getAppUserDataDirectory "whologin"
    return $ dirpath ++ [pathSeparator] ++ "whologin.conf"


-- | Main filepath for log files
pathToLog :: IO FilePath
pathToLog = do
    dirpath <- getAppUserDataDirectory "whologin"

    day <- currentDay
    let timestamp = showGregorian day

    return $ dirpath ++ [pathSeparator] ++ "log_" ++ timestamp



-- | Current time
currentTime = timeOfDay localTimeOfDay
-- | Current day
currentDay  = timeOfDay localDay

timeOfDay f = do
    zonedTime <- getZonedTime
    let localTime = zonedTimeToLocalTime zonedTime
    return $ f localTime



-- }}}

-- {{{ Arguments

-- | Get the resulting Options data
parseArgs :: [OptDescr (Options -> Options)] -> [String] -> Options -> Options
parseArgs options args defaults =
    let opts  (o,_,_)   = o
        other (_,n,_)   = n
        gets            = getOpt Permute options args

        newOpts         = foldl' (flip id) defaults $ opts gets
        -- we still have to get the name and pw from the "non-options"
        newName         = case length (other gets) of
                             2 -> Just $ other gets !! 0
                             _ -> name defaults
        newPw           = case length (other gets) of
                             2 -> Just $ other gets !! 1
                             _ -> pw defaults
    in newOpts { name = newName
               , pw   = newPw
               }

-- | Version & usage informations
showInfo = let name     = pName defaultOptions
               version  = pVersion defaultOptions
               header   = name ++ " " ++ show version ++ "\n\nBenutzung: [OPTIONEN] [NAME] [PASSWORT]"
           in
           putStrLn $ usageInfo header options

-- }}}

-- {{{ Login

-- | Login one time
runOnce :: Options -> IO Bool
runOnce opts = do
    run <- runWhoAuth (fromMaybe "" $ name opts) (fromMaybe "" $ pw opts)

    case run of
         Connected      -> return True  << putStrLn "  Bereits Angemeldet"
         LoginDone      -> return True  << logStrLn "> Erfolgreich angemeldet"
         Disconnected   -> return True  << logStrLn "! Anmeldung schlug fehl"
         Rejected       -> return False << logStrLn "Fehler: Falscher Benutzername / Passwort!" 
  where logStrLn =
            case logFile opts of
                 Just logpath   -> liftM2 (>>) (logToFile logpath) putStrLn -- log to file
                 Nothing        -> putStrLn -- no logfile: just print it

-- | Run login command every (Int) seconds while no error occures
background :: Options -> IO ()
background opts = do
    run <- runOnce opts
    if run
       then do
           let int = case runLevel opts of
                          Background int -> int
                          _ -> 60   -- I dont like this...

           threadDelay $ int * 10^6 -- int in seconds
           background opts

       else putStrLn "\nAbbruch!"


-- }}}

-- {{{ Config

-- | Load configuration
loadConfig :: FilePath -> Options -> IO Options
loadConfig fp opts = do
    -- get from config file
    interval    <- loadConf "NETWORK"   ["interval"]    fp
    uData       <- loadConf "LOGIN"     ["name","pw"]   fp

    -- turn the results into new values for our Options
    let newRunLevel = maybe (runLevel opts) (Background . read . flip (##) "interval") interval
        newName     = maybe (name     opts) (Just . flip (##) "name") uData
        newPw       = maybe (pw       opts) (Just . flip (##) "pw")   uData

    return $ opts   { runLevel = newRunLevel
                    , name = newName
                    , pw = newPw
                    }




-- | Start whoLogin with config file, prefer arguments
runWithConfig :: Options -> IO ()
runWithConfig opts =
    case runLevel opts of
         Once           -> putStrLn "Starte Login-Vorgang" >> void (runOnce opts)
         Configuration  -> startConfiguration opts
         Background int -> startBackground opts


-- | Start in Background
startBackground opts = do
    let int = case (runLevel opts) of Background int -> int; _ -> 60;
    putStrLn   "Start im Hintergrund"
    putStrLn $ "Verbindung alle " ++ show int ++ " Sekunden überprüfen.\n"
    background opts


-- | Interactive configuration of whoLogin
startConfiguration :: Options -> IO ()
startConfiguration opts = do
    putStrLn "Konfiguration gestartet"

    filepath <- pathToConf

    putStrLn $ "Dateipfad für Konfigurationsdatei: " ++ filepath

    let prompt str = putStr str >> hFlush stdout >> getLine
    name        <- prompt "Login-Name:     "
    pw          <- prompt "Passwort:       "
    interval    <- prompt "Interval:  [60] "

    save <- prompt "Diese Werte speichern?  [Y/n] "
    case save of
         "n" -> putStrLn "Abbruch!"
         _   -> do putStrLn "Speichern..."

                   -- create dirpath
                   dirpath <- getAppUserDataDirectory "whologin"
                   dirExists <- doesDirectoryExist dirpath
                   unless dirExists $ createDirectory dirpath

                   -- create a whologin.conf in filepath
                   fileExists <- doesFileExist filepath
                   unless fileExists $ writeFile filepath ""

                   -- save configuration
                   login    <- setConf "LOGIN"   [("name",name), ("pw",pw)] filepath
                   network  <- setConf "NETWORK" [("interval",if null interval then "60" else interval)] filepath

                   if login && network
                      then putStrLn "Erfolgreich konfiguriert"
                      else putStrLn "Fehler beim speichern der Konfigurationsdatei"




-- }}}

-- {{{ Other

-- | Ignore output
void :: Functor f => f a -> f ()
void = (()<$)

-- | Little helper :)
(<<) = flip (>>)

-- }}}
