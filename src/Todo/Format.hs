{-# LANGUAGE GADTs, QuasiQuotes, ScopedTypeVariables #-}
module Todo.Format
  ( format
  ) where

import Todo.Eval
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List (intercalate)
import Text.InterpolatedString.QM

format :: Either EvalException [Text] -> String
format (Left ex) = [qm|Error: {show ex}|]
format (Right exps) = format' exps

format' :: [Text] -> String
format' exps = intercalate "\n" $
  map (\(idx::Int, cur) -> [qm|~> {show idx}. {toString $ encodeUtf8 cur}|]) $ zip [0..] exps
