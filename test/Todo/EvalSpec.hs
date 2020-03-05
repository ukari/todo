module Todo.EvalSpec
  ( spec
  ) where

import Todo.Ast
import Todo.Eval
import Test.Hspec
import Data.Text.Lazy (Text, pack)

source :: [Exp Text]
source =
  [ Todo . Task $ pack "test 0"
  , Undo . Task $ pack "test 1"
  , Todo . Task $ pack "test 2"
  , Done . Task $ pack "test 0"
  , Todo . Task $ pack "test 3"
  , Done . Task $ pack "test 1"]

result :: [Text]
result = map pack ["test 2","test 3"]

spec :: Spec
spec = do
  describe "Eval" $ do
    it "test evaluate" $ do
      (evaluate source) `shouldBe` result 
