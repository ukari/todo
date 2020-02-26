module Todo.Command.Add
  ( add
  ) where

add :: FilePath -> String -> IO ()
add filepath task = print $ "add" <> task
