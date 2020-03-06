{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module Todo.Command.Version
  ( version
  ) where

import Todo.Logger (Log (..))
import qualified Todo.Logger as Logger
import Data.Version
import qualified Paths_todo as Todo
import Development.GitRev

version :: IO ()
version = Logger.log $ Info $ "Version "<> showVersion Todo.version <> ", Git " <> $(gitHash) <> ", " <> $(gitCommitCount) <>" commits, Date " <> $(gitCommitDate)
