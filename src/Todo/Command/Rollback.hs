module Todo.Command.Rollback
  ( rollback
  ) where

rollback :: FilePath -> IO ()
rollback filepath = print "rollback"
