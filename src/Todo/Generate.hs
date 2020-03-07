{-# LANGUAGE GADTs, QuasiQuotes, LambdaCase #-}

module Todo.Generate
  ( action
  , gene
  , generate
  ) where

import Todo.Ast
import Data.Text.Lazy (Text, empty, pack)
import qualified Data.Text.Lazy as Text
import Text.InterpolatedString.QM

isEscape :: Char -> Bool
isEscape = \case
  '\0' -> True -- U+0000 null character
  '\a' -> True -- U+0007 alert
  '\b' -> True -- U+0008 backspace
  '\f' -> True -- U+000C form feed
  '\n' -> True -- U+000A newline (line feed)
  '\r' -> True -- U+000D carriage return
  '\t' -> True -- U+0009 horizontal tab
  '\v' -> True -- U+000B vertical tab
  '\"' -> True -- U+0022 double quote
--'\&' -> True -- n/a    empty string
  '\'' -> True -- U+0027 single quote
  '\\' -> True -- U+005C backslash
  _ -> False

gene :: Exp Text -> Text
gene (Task t) = Text.foldl escape empty t
  where
    escape :: Text -> Char -> Text
    escape acc cur = if isEscape cur then acc <> pack ['\\', cur] else acc <> pack [cur]
gene (Todo e) = [qm|todo "{gene e}"\n|]
gene (Undo e) = [qm|undo "{gene e}"\n|]
gene (Done e) = [qm|done "{gene e}"\n|]

action :: Exp Text -> Text
action (Todo _) = pack "add"
action (Done _) = pack "fine"
action _ = error "unsupport action"

generate :: [Exp Text] -> Text
generate exps = foldl f empty exps
  where
    f :: Text -> Exp Text -> Text
    f acc cur = acc <> gene cur
