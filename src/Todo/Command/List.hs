module Todo.Command.List
  ( list
  ) where

import Todo.Parse
import Todo.Eval
import Todo.Formatter
import qualified Todo.Logger as Logger

list :: FilePath -> IO ()
list filepath = do
  ast <- parseFile filepath
  exps <- return $ evaluate ast
  Logger.log $ format exps
