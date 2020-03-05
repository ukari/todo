module Todo.EvalSpec
  ( spec
  ) where

import Todo.Ast
import Todo.Eval
import Test.Hspec
import Data.Text.Lazy (Text, pack)

source :: [Exp Text]
source =
  [ Todo $ Task $ pack "test 0"
  , Undo $ Task $ pack "test 1"
  , Todo $ Task $ pack "test 2"
  , Done $ Task $ pack "test 0"
  , Todo $ Task $ pack "test 3"
  , Done $ Task $ pack "test 1" ]

source_bad :: [Exp Text]
source_bad =
  [ Todo $ Task $ pack "task 0"
  , Done $ Task $ pack "task 0"
  , Todo $ Task $ pack "task 0"
  , Undo $ Task $ pack "task 0" ]

spec :: Spec
spec = do
  describe "Eval" $ do
    it "normal evaluate" $ do
      (evaluate source) `shouldBe` (Right $ map pack ["test 2","test 3"])

    it "illegal undo action" $ do
      (evaluate source_bad) `shouldBe` Left IllegalUndoException
