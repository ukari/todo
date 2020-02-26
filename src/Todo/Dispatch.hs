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
import Options.Applicative
import Numeric.Natural
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
import System.IO
import System.Posix.User
import Options.Applicative.Types
import Data.Maybe

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

todo :: Parser Todo
todo = Todo <$> (optional $ strOption
                (  long "source"
                <> short 's'
                <> metavar "FILE"
                <> help ("Specify a storage file")))
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
fineCommand = command "fine" (info (Fine <$> many (argument auto (metavar "INDEX" <> showDefault <> help "Task index"))
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
  filepath <- sourceHandler source
  checkSource filepath
  commandDispatch filepath cmd

sourceHandler :: Maybe FilePath -> IO (String)
sourceHandler (Just filepath) = return filepath
sourceHandler Nothing = do
  UserEntry {homeDirectory} <- getLoginName >>= getUserEntryForName
  return $ homeDirectory <> "/.todo"

checkSource :: FilePath -> IO ()
checkSource filepath = do
  print filepath

commandDispatch :: String -> Command -> IO ()
commandDispatch source (Add task) = add source task
commandDispatch source List = list source
commandDispatch source (Fine {idx, isAll}) = fine source idx isAll
commandDispatch source Rollback = rollback source
commandDispatch source Gc = gc source
commandDispatch _ Version = version
