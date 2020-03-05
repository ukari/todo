module Todo.Formatter
  ( format
  ) where

import Todo.Eval
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List (intercalate)

format :: Either EvalException [Text] -> String
format (Left ex) = show ex
format (Right exps) = intercalate "\n" $
  map (\(idx, cur) -> "~> " <> (show (idx::Int)) <> ". " <> (toString $ encodeUtf8 cur)) $ zip [0..] exps
