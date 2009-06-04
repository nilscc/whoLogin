module Config   ( loadConf
                , setConf
                , (##)
                ) where


import Data.ConfigFile

import Control.Monad (join, foldM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Error (runErrorT)

import System.Directory (doesFileExist)

type ConfData = (String, String)



-- | Get value of a given key
-- analog to @lookup :: Eq a => a -> [(a, b)] -> Maybe b@ or @[a] !! b@
(##) :: [ConfData] -> String -> String
xs ## key = case lookup key xs of
                 Nothing -> ""
                 Just a  -> a


-- | Load (key,value) pairs from section of a configfile
-- Uses 'getOneValue' to map over the given list
loadConf :: String -> [String] -> FilePath -> IO (Maybe [ConfData])
loadConf sec keys fp = do
    pairs <- mapM (liftIO . flip (getOneValue sec) fp) keys
    return $ case foldr dropNothing [] pairs of
                  [] -> Nothing
                  a  -> Just a
  where dropNothing Nothing  xs = xs
        dropNothing (Just x) xs = x : xs


-- | Load one (key,value) pair from section of the configfile
getOneValue :: String -> String -> FilePath -> IO (Maybe ConfData)
getOneValue sec key fp = do
    fileExists <- doesFileExist fp
    if not fileExists
       then return Nothing
       else do cp <- runErrorT $ do
                     x   <- join . liftIO $ readfile emptyCP fp
                     val <- get x sec key
                     return val
               return $ case cp of
                             Right val  -> Just (key,val)
                             _          -> Nothing


-- | Set value of a given key of section of a config file
setConf :: String -> [ConfData] -> FilePath -> IO Bool
setConf sec list fp = do
    fileExists <- doesFileExist fp
    if not fileExists
       then return False
       else do cp <- runErrorT $ do
                     x <- join . liftIO $ readfile emptyCP fp
                     x <- if has_section x sec
                             then return x
                             else add_section x sec
                     x <- foldM (\x (k,v) -> set x sec k v) x list
                     return $ to_string x
               case cp of
                    Right c -> writeFile fp c >> return True
                    _       -> return False
