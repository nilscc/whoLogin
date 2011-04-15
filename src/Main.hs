{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module Main where

import Control.Monad
import Control.Concurrent
import Data.Time

import qualified Control.Exception as E

import Config
import Connect
import Server
import Types

main :: IO ()
main = do
    c  <- loadConfig
    m  <- newMVar c
    ct <- getZonedTime
    l  <- newMVar [(ct,"Starting up.")]

    let
        s                = State l m

        -- See if 'x' is the last message in the 'xs' log list
        isLastIn _ []    = False
        isLastIn x xs    = case last xs of (_,x') -> x == x'

        -- Catch and log all exceptions
        logExceptions io = io `E.catch` \(e :: E.SomeException) -> logErr (show e)

        -- Log a new error. If the same error occurrs two times in a row, it's
        -- logged only once
        logErr err       = do let err' = "Error: " ++ err
                              ls <- readMVar l
                              unless (err' `isLastIn` ls)
                                     (logMsg err')

        -- Log any message to the log MVar
        logMsg msg       = do ct' <- getZonedTime
                              modifyMVar_ l $ \ls -> return $ ls ++ [(ct',msg)]

    forkIO $ runServer s

    loggedIn <- isLoggedIn
    when loggedIn $ logMsg "Already logged in."

    forever $ do
        loggedIn' <- isLoggedIn
        Config { login, password, passArty5, interval } <- readMVar m
        unless loggedIn' $ logExceptions $ do
            r <- performLogin login password passArty5
            case r of
                 Right (Just msg) -> logMsg msg
                 Right Nothing    -> return ()
                 Left err         -> logErr err
        threadDelay (1000000 * interval)
