module Todo.Parse
  ( parseFile
  ) where

import Todo.Ast
import Text.Megaparsec
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as T
import Data.Void
import Control.Monad.IO.Class (MonadIO, liftIO)

parseFile :: MonadIO m => FilePath -> m ([Exp Text])
parseFile filepath = liftIO $ do
  source <- T.readFile filepath
  result <- return $ parse expsParser filepath source
  return $ f result
  where
    f :: Either (ParseErrorBundle Text Void) [Exp Text] -> [Exp Text]
    f (Left x) = error $ errorBundlePretty x
    f (Right x) = x
