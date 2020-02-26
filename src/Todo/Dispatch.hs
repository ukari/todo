{-# LANGUAGE NamedFieldPuns #-}

module Todo.Dispatch
  ( dispatch
  ) where

import Todo.Command.Add
import Todo.Command.List
import Todo.Command.Fine
import Todo.Command.Gc
import Todo.Command.Version
import Options.Applicative
import Numeric.Natural

data Command
  = Add !String
  | List
  | Fine !Natural
  | Gc
  | Version
  deriving (Eq, Show)

data Todo = Todo
  { opts :: !String
  , cmd :: !Command
  } deriving (Eq, Show)

todo :: Parser Todo
todo = Todo
  <$> strOption
      ( long "source"
     <> short 's'
     <> metavar "FILE"
     <> value "~/.todo"
     <> help "Specify a storage file")
  <*> (hsubparser $ addCommand
                 <> listCommand
                 <> fineCommand
                 <> gcCommand
                 <> versionCommand)

todoOptions :: ParserInfo Todo
todoOptions = info (todo <**> helper) idm

addCommand :: Mod CommandFields Command
addCommand = command "add" (info (Add <$> strArgument (metavar "TASK" <> help "Task for finishing")) (progDesc "Add a todo task"))

listCommand :: Mod CommandFields Command
listCommand = command "list" (info (pure List) (progDesc "List all unfinished tasks with index"))

fineCommand :: Mod CommandFields Command
fineCommand = command "fine" (info (Fine <$> argument auto (metavar "INDEX" <> showDefault <> help "Task index")) (progDesc "Finish a task specify by index"))

gcCommand :: Mod CommandFields Command
gcCommand = command "gc" (info (pure Gc) (progDesc "Collect garbage, which would clean all unused history"))

versionCommand :: Mod CommandFields Command
versionCommand = command "version" (info (pure Version) (progDesc "Print version"))

dispatch :: IO ()
dispatch = do
  Todo {opts, cmd} <- execParser todoOptions
  commandDispatch opts cmd

commandDispatch :: String -> Command -> IO ()
commandDispatch source (Add task) = add source task
commandDispatch source (List) = list source
commandDispatch source (Fine idx) = fine source idx
commandDispatch source (Gc) = gc source
commandDispatch _ (Version) = version
