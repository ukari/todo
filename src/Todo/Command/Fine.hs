module Todo.Command.Fine
  ( fine
  ) where

import Numeric.Natural

fine :: FilePath -> [Natural] -> Bool -> IO ()
fine filepath index isAll = print $ "fine " <> show index
