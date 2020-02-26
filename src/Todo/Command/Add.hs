module Todo.Command.Add
  ( add
  ) where

import Options.Applicative

add :: FilePath -> String -> IO ()
add filepath task = print ['a','d', 'd']
