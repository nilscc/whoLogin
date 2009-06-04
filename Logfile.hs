module Logfile  (logToFile) where


import System.Directory (doesFileExist)
import Control.Monad (when)

-- | Append string to file
logToFile :: FilePath -> String -> IO ()
logToFile fp str = do
    exists <- doesFileExist fp
    when exists $ appendFile fp str
