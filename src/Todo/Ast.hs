{-# LANGUAGE GADTs, StandaloneDeriving, OverloadedStrings, RecordWildCards #-}

module Todo.Ast
  ( Exp (..)
  , expsParser
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text.Lazy (Text)
import Data.Void
import Control.Monad

data Exp r where
  Task :: Text -> Exp Text
  Todo :: Exp r -> Exp r
  Undo :: Exp r -> Exp r
  Done :: Exp Text -> Exp Text

deriving instance Show r => Show (Exp r)

type Parser = Parsec Void Text

manyLineSpace :: Parser ()
manyLineSpace = void $ takeWhileP Nothing f
  where
    f :: Char -> Bool
    f x = (x == ' ') || (x == '\n') || (x == '\r') || (x == '\t')

taskParser :: Parser (Exp Text)
taskParser = Task <$> (char '\"' *> (takeWhile1P Nothing (/= '\"')) <* char '\"')

todoParser :: Parser (Exp Text)
todoParser = do
  todo <- string "todo"
  _ <- manyLineSpace
  task <- taskParser
  return $ Todo task

undoParser :: Parser (Exp Text)
undoParser = do
  undo <- string "undo"
  _ <- manyLineSpace
  task <- taskParser
  return $ Undo task

doneParser :: Parser (Exp Text)
doneParser = do
  done <- string "done"
  _ <- manyLineSpace
  task <- taskParser
  return $ Done task

expParser :: Parser (Exp Text)
expParser = todoParser <|> undoParser <|> doneParser

expsParser :: Parser [Exp Text]
expsParser = do
  h <- optional expParser
  t <- manyTill (manyLineSpace *> expParser <* manyLineSpace) eof
  return $ f h t
  where
    f :: Maybe (Exp Text) -> [Exp Text] -> [Exp Text]
    f (Just he) ta = he : ta
    f Nothing ta = ta
