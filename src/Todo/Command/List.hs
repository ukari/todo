module Todo.Command.List
  ( list
  ) where

list :: FilePath -> IO ()
list filepath = print ['l', 'i', 's', 't']
