{-# LANGUAGE GADTs #-}

module Todo.Eval
  ( EvalException (..)
  , evaluate
  ) where

import Todo.Ast
import Data.Text.Lazy (Text)
import Data.Sequence
import Data.Foldable
import Control.Exception (Exception)
import Control.Monad

plain :: Exp Text -> Text
plain (Task t) = t
plain (Todo e) = plain e
plain (Undo e) = plain e
plain (Done e) = plain e

data EvalException
  = UnexceptedTaskException
  | IllegalTodoException
  | IllegalUndoException
  | IllegalDoneException
  deriving (Show)

instance Exception EvalException

check :: Exp Text -> Seq Text -> Either EvalException (Exp Text)
check (Task t) _ = Left UnexceptedTaskException
check (Todo e) exps = case (elemIndexL (plain e) exps) of
  Nothing -> Right (Todo e)
  Just _ -> Left IllegalTodoException
check (Undo e) exps = case (elemIndexL (plain e) exps) of
  Nothing -> Right (Undo e)
  Just _ -> Left IllegalUndoException
check (Done e) exps = case (elemIndexL (plain e) exps) of
  Just _ -> Right (Done e)
  Nothing -> Left IllegalDoneException

eval :: Exp Text -> Seq Text -> Seq Text
eval (Task t) exps = exps |> t
eval (Todo e) exps = eval e exps
eval (Undo e) exps = eval e exps
eval (Done e) exps = done (elemIndexL (plain e) exps) exps
  where
    done :: Maybe Int -> Seq Text -> Seq Text
    done (Just idx) s = deleteAt idx s
    done Nothing s = s

evaluate :: [Exp Text] -> Either EvalException [Text]
evaluate xs = do
  case (foldM f empty xs) of
    Left x -> Left x
    Right x -> Right $ toList x
  where
    f :: Seq Text -> Exp Text -> Either EvalException (Seq Text)
    f acc cur = do
      action <- (check cur acc)
      return $ eval action acc
