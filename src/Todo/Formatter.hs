module Todo.Formatter
  ( format
  ) where

import Todo.Eval
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.ByteString.Lazy.UTF8 (toString)

format :: Either EvalException [Text] -> String
format (Left ex) = show ex <> "\n"
format (Right exps) = foldl (\acc (idx, cur) -> acc <> "~> " <> (show (idx::Int)) <> ". " <> (toString $ encodeUtf8 cur) <> "\n") "" $ zip [0..] exps
