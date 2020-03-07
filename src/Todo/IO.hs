module Todo.IO
  ( detect
  , defaultStorageName
  , storagePath
  , checkStorage
  ) where

import System.Posix (FileMode, fileExist, createFile, closeFd, unionFileModes, ownerReadMode, ownerWriteMode, groupReadMode, groupWriteMode, otherReadMode)
import Path (Path, Abs, File, Dir, parent, toFilePath)
import Path.IO (getCurrentDir, getHomeDir, resolveFile)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (when)

defaultStorageName :: String
defaultStorageName = ".todo"

detect :: MonadIO m => Maybe FilePath -> m FilePath
detect (Just filepath) = return filepath
detect Nothing = do
  home <- getHomeDir
  current <- getCurrentDir
  path <- detect' current home
  return $ toFilePath path

storagePath :: MonadIO m => Path Abs Dir -> m (Path Abs File)
storagePath dir = resolveFile dir defaultStorageName

detect' :: MonadIO m => Path Abs Dir -> Path Abs Dir -> m (Path Abs File)
detect' cur top = do
  path <- storagePath cur
  exist <- liftIO $ fileExist $ toFilePath $ path
  if
    (cur /= top) && (not exist)
  then
    detect' (parent cur) top
  else
    storagePath cur

checkStorage :: FilePath -> IO (Bool)
checkStorage source = do
  exist <- fileExist source
  when (not exist) $ do
    initStorage source
  return exist

initStorage :: FilePath -> IO ()
initStorage filepath = createFile filepath filemode >>= closeFd
  where
    filemode :: FileMode
    filemode = foldr1 unionFileModes [ownerReadMode, ownerWriteMode, groupReadMode, groupWriteMode, otherReadMode]
