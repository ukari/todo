{-# LANGUAGE GADTs, QuasiQuotes #-}

module Todo.Generate
  ( action
  , gene
  , generate
  ) where

import Todo.Ast
import Data.Text.Lazy (Text, empty, pack)
import Text.InterpolatedString.QM

gene :: Exp Text -> Text
gene (Task t) = t
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
