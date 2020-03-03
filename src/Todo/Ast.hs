{-# LANGUAGE GADTs, StandaloneDeriving, OverloadedStrings, RecordWildCards #-}

module Todo.Ast
  (
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Text.Lazy (Text, pack)
import Data.Void
import Data.Text.Lazy.IO as T
import Control.Applicative (Alternative)
import Control.Monad

data Exp r where
  Task :: Text -> Exp Text
  Todo :: Exp r -> Exp r
  Undo :: Exp r -> Exp r
  Done :: Exp Text -> Exp Text

deriving instance Show r => Show (Exp r)

type Parser = Parsec Void Text

sc :: Parser ()
sc = Lexer.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

manySpace :: Parser ()
manySpace = void $ takeWhile1P Nothing (== ' ')

taskParser :: Parser (Exp Text)
taskParser = Task <$> (char '\"' *> (takeWhile1P Nothing (/= '\"')))

todoParser :: Parser (Exp Text)
todoParser = do
  todo <- string "todo"
  _ <- manySpace
  task <- taskParser
  return $ Todo task

undoParser :: Parser (Exp Text)
undoParser = do
  undo <- string "undo"
  _ <- manySpace
  task <- taskParser
  return $ Undo task

doneParser :: Parser (Exp Text)
doneParser = do
  done <- string "done"
  _ <- manySpace
  task <- taskParser
  return $ Done task

expParser :: Parser [Exp Text]
expParser = many ((todoParser <|> undoParser <|> doneParser))
