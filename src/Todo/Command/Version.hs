{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}

module Todo.Command.Version
  ( version
  ) where

import Data.Version
import qualified Paths_todo as Todo
import Development.GitRev

version :: IO ()
version = putStrLn $ "Version "<> showVersion Todo.version <> ", Git " <> $(gitHash) <> ", " <> $(gitCommitCount) <>" commits, Date " <> $(gitCommitDate)
