{-# LANGUAGE QuasiQuotes #-}

module Todo.Command.Gc
  ( gc
  ) where

import Todo.Ast
import Todo.Parse
import Todo.Eval
import Todo.Generate
import Todo.Logger (Log (..))
import qualified Todo.Logger as Logger
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T
import Data.Either.Combinators
import Text.InterpolatedString.QM
import Data.List (genericLength)

gc :: FilePath -> IO ()
gc filepath = do
  ast <- parseFile filepath
  exps <- return $ evaluate $ ast
  whenRight exps $ gc' filepath
  ast_after <- parseFile filepath
  gcReport $ (genericLength ast) - (genericLength ast_after)

gc' :: FilePath -> [Text] -> IO ()
gc' filepath ts = T.writeFile filepath $ generate $ map (Undo . Task) ts

gcReport :: Integer -> IO ()
gcReport i = Logger.log $ Info $ [qm|[{i} history actions] collected.|]
