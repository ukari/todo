module Todo.Command.Fine
  ( fine
  ) where

import Numeric.Natural

fine :: FilePath -> [Natural] -> Bool -> IO ()
fine filepath index isAll = print $ ['f','i','n', 'e'] <> show index
