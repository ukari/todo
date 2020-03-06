{-# LANGUAGE QuasiQuotes #-}

module Todo.Command.Fine
  ( fine
  ) where

import Todo.Ast
import Todo.Parse
import Todo.Eval
import Todo.Format
import Todo.Generate
import qualified Todo.Logger as Logger
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T
import Text.InterpolatedString.QM
import Data.List (genericLength)
import Data.Set ((\\), intersection, toList, fromList)

fine :: FilePath -> [Integer] -> Bool -> IO ()
fine filepath index isAll = do
  ast <- parseFile filepath
  eexps <- return $ evaluate ast
  case eexps of
    Left _ -> Logger.log $ format eexps
    Right exps -> do
      allset <- return $ fromList $ map (uncurry const) $ zip [0::Integer ..] exps
      iset <- return $ if isAll then allset else fromList index 
      illegals <- return $ toList $ iset \\ allset
      mapM_ fineReportE illegals
      legals <- return $ toList $ iset `intersection` allset
      mapM_ (T.appendFile filepath . fine1' exps) legals
      fineReport $ genericLength legals

fine1' :: [Text] -> Integer -> Text
fine1' exps idx = gene $ Done $ Task $ exps !! fromIntegral idx

fineReport :: Integer -> IO ()
fineReport i = Logger.log $ [qm|[{i} tasks] has been fine.|]

fineReportE :: Integer -> IO ()
fineReportE i = Logger.log $ [qm|Error task number {i}.|]
