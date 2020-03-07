{-# LANGUAGE QuasiQuotes #-}

module Todo.Command.Init
  ( init
  ) where

import Todo.IO
import Todo.Logger (Log (..))
import qualified Todo.Logger as Logger
import Prelude hiding (init)
import Path (toFilePath)
import Path.IO (getCurrentDir)
import Text.InterpolatedString.QM
import Control.Monad (when)

init :: IO ()
init = do
  current <- getCurrentDir
  path <- storagePath current
  filepath <- return $ toFilePath path
  exist <- checkStorage filepath
  when (not exist) $ do
    Logger.log $ Info $ [qm|Initialize a new todo storage file in {current}|]
