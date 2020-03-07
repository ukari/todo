{-# LANGUAGE NamedFieldPuns #-}

module Todo.Dispatch
  ( dispatch
  ) where

import Todo.Command.Add
import Todo.Command.List
import Todo.Command.Fine
import Todo.Command.Gc
import Todo.Command.Rollback
import Todo.Command.Version
import Todo.Logger (Log (..))
import qualified Todo.Logger as Logger
import Options.Applicative
import Numeric.Natural
import System.Posix (FileMode, fileExist, createFile, closeFd, unionFileModes, ownerReadMode, ownerWriteMode, groupReadMode, groupWriteMode, otherReadMode)
import Path (Path, Abs, File, Dir, parent, toFilePath)
import Path.IO (getCurrentDir, getHomeDir, resolveFile)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)

data Command
  = Add !String
  | List
  | Fine {idx :: ![Natural] , isAll :: !Bool}
  | Rollback
  | Gc
  | Version
  deriving (Eq, Show)

data Todo = Todo
  { source :: !(Maybe String)
  , cmd :: !Command
  } deriving (Eq, Show)

defaultSourceName :: String
defaultSourceName = ".todo"

todo :: Parser Todo
todo = Todo <$> (optional $ strOption
                (  long "source"
                <> short 's'
                <> metavar "FILE"
                <> help ("Specify a storage file. (Default) Find " <> defaultSourceName <> " from the current directory to the parent directory until the user home.") ))
            <*> (hsubparser
                $  addCommand
                <> listCommand
                <> fineCommand
                <> rollbackCommand
                <> gcCommand
                <> versionCommand)

todoOptions :: ParserInfo Todo
todoOptions = info (todo <**> helper) idm

addCommand :: Mod CommandFields Command
addCommand = command "add" (info (Add <$> strArgument (metavar "TASK" <> help "Task for finishing")) (progDesc "Add a todo task"))

listCommand :: Mod CommandFields Command
listCommand = command "list" (info (pure List) (progDesc "List all unfinished tasks with index"))

fineCommand :: Mod CommandFields Command
fineCommand = command "fine" (info (Fine <$> many (argument auto (metavar "INDEX.." <> showDefault <> help "Task index"))
                                         <*> switch (long "all" <> short 'A' <> help "Fine all unfinished tasks"))
                               (progDesc "Finish a task specify by index"))

rollbackCommand :: Mod CommandFields Command
rollbackCommand = command "rollback" (info (pure Rollback) (progDesc "Rollback permanently, can't do this after gc"))

gcCommand :: Mod CommandFields Command
gcCommand = command "gc" (info (pure Gc) (progDesc "Collect garbage, which would clean all unused history"))

versionCommand :: Mod CommandFields Command
versionCommand = command "version" (info (pure Version) (progDesc "Print version"))

dispatch :: IO ()
dispatch = do
  Todo {source, cmd} <- execParser todoOptions
  filepath <- detect source
  checkSource filepath
  commandDispatch filepath cmd

detect :: MonadIO m => Maybe FilePath -> m FilePath
detect (Just filepath) = return filepath
detect Nothing = do
  home <- getHomeDir
  current <- getCurrentDir
  path <- detect' current home
  return $ toFilePath path

sourcePath :: MonadIO m => Path Abs Dir -> m (Path Abs File)
sourcePath dir = resolveFile dir defaultSourceName

detect' :: MonadIO m => Path Abs Dir -> Path Abs Dir -> m (Path Abs File)
detect' cur top = do
  path <- sourcePath cur
  exist <- liftIO $ fileExist $ toFilePath $ path
  if
    (cur /= top) && (not exist)
  then
    detect' (parent cur) top
  else
    sourcePath cur

checkSource :: FilePath -> IO ()
checkSource source = do
  exist <- fileExist source
  when (not exist) $ do
    Logger.log $ Info $ "Source FILE not exist, create " <> source
    createFile source filemode >>= closeFd
  where
    filemode :: FileMode
    filemode = foldr1 unionFileModes [ownerReadMode, ownerWriteMode, groupReadMode, groupWriteMode, otherReadMode]

commandDispatch :: String -> Command -> IO ()
commandDispatch source (Add task) = add source task
commandDispatch source List = list source
commandDispatch source (Fine {idx, isAll}) = fine source (map fromIntegral idx) isAll
commandDispatch source Rollback = rollback source
commandDispatch source Gc = gc source
commandDispatch _ Version = version
