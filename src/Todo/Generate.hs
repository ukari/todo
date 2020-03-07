{-# LANGUAGE GADTs, QuasiQuotes #-}

module Todo.Generate
  ( action
  , gene
  , generate
  ) where

import Todo.Ast
import Data.Text.Lazy (Text, empty, pack)
import qualified Data.Text.Lazy as Text
import Text.InterpolatedString.QM

escapeChars :: [Char]
escapeChars = [ '\0'
              , '\a'
              , '\b'
              , '\f'
              , '\n'
              , '\r'
              , '\t'
              , '\v'
              , '\"'
            --, '\&' means empty string
              , '\''
              , '\\']

gene :: Exp Text -> Text
gene (Task t) = Text.foldl escape empty t
  where
    escape :: Text -> Char -> Text
    escape acc cur = if elem cur escapeChars then acc <> pack ['\\', cur] else acc <> pack [cur]
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
