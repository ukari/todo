module Todo.Parse
  ( parse
  , parseFile
  ) where

import Todo.Ast
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import qualified Text.Megaparsec as Parsec
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as T
import Data.Void
import Control.Monad.IO.Class (MonadIO, liftIO)

parseFile :: MonadIO m => FilePath -> m ([Exp Text])
parseFile filepath = liftIO $ do
  source <- T.readFile filepath
  parse source filepath

parse :: MonadIO m => Text -> FilePath -> m ([Exp Text])
parse source filepath = do
  result <- return $ Parsec.parse expsParser filepath source
  return $ f result
  where
    f :: Either (ParseErrorBundle Text Void) [Exp Text] -> [Exp Text]
    f (Left x) = error $ errorBundlePretty x
    f (Right x) = x
