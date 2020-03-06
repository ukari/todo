{-# LANGUAGE QuasiQuotes #-}

module Todo.Logger
  ( Log (..)
  , log
  ) where

import Prelude hiding (log)
import Text.InterpolatedString.QM

data Log
  = Info !String
  | Error !String
  deriving (Eq, Show)

log :: Log -> IO ()
log (Info m) = putStrLn m
log (Error m) = putStrLn [qm|Error: {m}|]
