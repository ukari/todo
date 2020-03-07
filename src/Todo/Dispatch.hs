{-# LANGUAGE NamedFieldPuns #-}

module Todo.Dispatch
  ( dispatch
  ) where

import Todo.Command.Add
import Todo.Command.List
import Todo.Command.Fine
import Todo.Command.Gc
import Todo.Command.Rollback
import qualified Todo.Command.Init as Init
import Todo.Command.Version
import Todo.Detect
import Todo.Logger (Log (..))
import qualified Todo.Logger as Logger
import Options.Applicative
import Numeric.Natural
import Control.Monad (when)

data Command
  = Add !String
  | List
  | Fine {idx :: ![Natural] , isAll :: !Bool}
  | Rollback
  | Gc
  | Init
  | Version
  deriving (Eq, Show)

data Todo = Todo
  { source :: !(Maybe String)
  , cmd :: !Command
  } deriving (Eq, Show)

todo :: Parser Todo
todo = Todo <$> (optional $ strOption
                (  long "source"
                <> short 's'
                <> metavar "FILE"
                <> help ("Specify a storage file") ))
            <*> (hsubparser
                $  addCommand
                <> listCommand
                <> fineCommand
                <> rollbackCommand
                <> gcCommand
                <> initCommand
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

initCommand :: Mod CommandFields Command
initCommand = command "init" (info (pure Init) (progDesc $ "Initialize a storage file " <> defaultStorageName <> " in the local directory if not exist"))

versionCommand :: Mod CommandFields Command
versionCommand = command "version" (info (pure Version) (progDesc "Print version"))

dispatch :: IO ()
dispatch = do
  Todo {source, cmd} <- execParser todoOptions
  filepath <- detect source
  commandDispatch filepath cmd

check :: FilePath -> IO ()
check filepath = do
  exist <- checkStorage filepath
  when (not exist) $ do
    Logger.log $ Info $ "Source FILE not exist, create " <> filepath

commandDispatch :: FilePath -> Command -> IO ()
commandDispatch source (Add task) = check source >> add source task
commandDispatch source List = check source >> list source
commandDispatch source (Fine {idx, isAll}) = check source >> fine source (map fromIntegral idx) isAll
commandDispatch source Rollback = check source >> rollback source
commandDispatch source Gc = check source >> gc source
commandDispatch _ Init = Init.init
commandDispatch _ Version = version
