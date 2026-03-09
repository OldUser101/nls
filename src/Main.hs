module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Eval as E
import qualified Parser as P
import qualified Repl as R
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      t <- TIO.readFile filePath
      case P.parse t of
        Left err -> putStrLn (T.unpack err)
        Right ast -> do
          case E.evalProgram E.baseEnv ast of
            Left err -> putStrLn (T.unpack err)
            Right result -> mapM_ print result
    _ -> R.repl E.baseEnv
