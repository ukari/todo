{-# LANGUAGE GADTs, StandaloneDeriving, OverloadedStrings, RecordWildCards, LambdaCase #-}

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

expParser :: Parser (Exp Text)
expParser = do
  action <- choice
    [ Todo <$ string "todo"
    , Undo <$ string "undo"
    , Done <$ string "done" ]
  void $ manyLineSpace
  task <- taskParser
  return $ action task

expsParser :: Parser [Exp Text]
expsParser = do
  h <- optional expParser
  t <- manyTill (manyLineSpace *> expParser <* manyLineSpace) eof
  return $ f h t
  where
    f :: Maybe (Exp Text) -> [Exp Text] -> [Exp Text]
    f (Just he) ta = he : ta
    f Nothing ta = ta
