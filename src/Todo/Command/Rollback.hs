{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

module Todo.Command.Rollback
  ( rollback
  ) where

import Todo.Ast
import Todo.Parse
import Todo.Generate
import Todo.Logger (Log (..))
import qualified Todo.Logger as Logger
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T
import Text.InterpolatedString.QM
import Data.List (genericLength)

rollback :: FilePath -> IO ()
rollback filepath = do
  ast <- parseFile filepath
  if
    (length ast > 0)
  then do
    latest <- return $ last ast
    tryRoll latest ast
  else
    Logger.log $ Error $ "no actions left to rollback."
  where
    tryRoll :: Exp Text -> [Exp Text] -> IO ()
    tryRoll (Undo _) _ = Logger.log $ Error $ "can't rollback."
    tryRoll l ast = do
      T.writeFile filepath $ generate $ take ((genericLength ast) - 1) ast
      Logger.log $ Info $ [qm|Latest [{action l}] action revert.|]
