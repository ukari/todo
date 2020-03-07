{-# LANGUAGE QuasiQuotes #-}

module Todo.Command.Init
  ( init
  ) where

import Todo.Detect
import Todo.Logger (Log (..))
import qualified Todo.Logger as Logger
import Prelude hiding (init)
import Path (toFilePath)
import Path.IO (getCurrentDir, getHomeDir)
import Text.InterpolatedString.QM
import Control.Monad (when)

init :: IO ()
init = do
  current <- getCurrentDir
  home <- getHomeDir
  path <- storagePath $ if current < home
    then home
    else current
  filepath <- return $ toFilePath path
  exist <- checkStorage filepath
  when (not exist) $ do
    Logger.log $ Info $ [qm|Initialize a new todo storage file in {current}|]
