{-# LANGUAGE GADTs, OverloadedStrings, FlexibleInstances, StandaloneDeriving #-}

module Todo.Ast
  ( Exp (..)
  , expsParser
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Void
import Control.Monad

data Exp r where
  Task :: Text -> Exp Text
  Todo :: Show r => Exp r -> Exp r
  Undo :: Show r => Exp r -> Exp r
  Done :: Show r => Exp r -> Exp r

instance Show (Exp Text) where
  show (Task t) = "Task " <> (toString $ encodeUtf8 $ t)
  show (Todo e) = "Todo ("<> show e <> ")"
  show (Undo e) = "Undo ("<> show e <> ")"
  show (Done e) = "Done ("<> show e <> ")"

deriving instance Eq (Exp Text)
deriving instance Ord (Exp Text)

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
  void $ manyLineSpace
  return $ action task

expsParser :: Parser [Exp Text]
expsParser = do
  void $ manyLineSpace
  t <- manyTill expParser eof
  return t
