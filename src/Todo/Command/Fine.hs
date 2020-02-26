module Todo.Command.Fine
  ( fine
  ) where

import Numeric.Natural

fine :: FilePath -> Natural -> IO ()
fine filepath index = print $ ['f','i','n', 'e'] <> show index
