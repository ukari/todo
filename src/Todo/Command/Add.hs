module Todo.Command.Add
  ( add
  ) where

import Todo.Ast
import Todo.Parse
import Todo.Eval
import Todo.Format
import qualified Todo.Logger as Logger
import Data.Text.Lazy (pack)
import Data.Text.Lazy.IO

add :: FilePath -> String -> IO ()
add filepath task = do
  ast <- parseFile filepath
  t <- return $ pack task
  exps <- return $ evaluate ast
  Logger.log $ format exps
  print $ "add" <> task
