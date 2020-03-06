module Todo.Command.Add
  ( add
  ) where

import Todo.Ast
import Todo.Parse
import Todo.Eval
import Todo.Format
import Todo.Generate
import qualified Todo.Logger as Logger
import Data.Text.Lazy (pack)
import qualified Data.Text.Lazy.IO as T
import Data.Either.Combinators

add :: FilePath -> String -> IO ()
add filepath task = do
  todo <- return $ Todo $ Task $ pack task
  ast <- parseFile filepath
  exps <- return $ evaluate $ ast <> [todo]
  whenRight exps (\_ -> T.appendFile filepath $ gene todo)
  Logger.log $ format exps
