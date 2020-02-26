module Todo.Command.Gc
  ( gc
  ) where

gc :: FilePath -> IO ()
gc filepath = print ['g','c']
