module Todo.Command.Gc
  ( gc
  ) where

gc :: FilePath -> IO ()
gc filepath = do
  print filepath
  print ['g','c']
