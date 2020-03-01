module Todo.Logger
  ( log
  ) where

import Prelude hiding (log)

log :: String -> IO ()
log = putStrLn
