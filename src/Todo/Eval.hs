{-# LANGUAGE GADTs #-}

module Todo.Eval
  ( evaluate
  ) where

import Todo.Ast
import Data.Text.Lazy (Text, pack)
import Data.Sequence

plain :: Exp Text -> Text
plain (Task t) = t
plain (Todo e) = plain e
plain (Undo e) = plain e
plain (Done e) = plain e

eval :: Exp Text -> Seq Text -> Seq Text
eval (Task t) exps = exps |> t
eval (Todo e) exps = eval e exps
eval (Undo e) exps = eval e exps
eval (Done e) exps = done (elemIndexL (plain e) exps) exps
  where
    done :: Maybe Int -> Seq Text -> Seq Text
    done (Just idx) s = deleteAt idx s
    done Nothing s = s

evaluate :: [Exp Text] -> Seq Text
evaluate xs = foldl f empty xs
  where
    f :: Seq Text -> Exp Text -> Seq Text
    f acc cur = eval cur acc
