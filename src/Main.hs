module Main (main) where

import qualified Data.Text as T
import qualified Eval as E
import qualified Parser as P
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName

  case args of
    [expr] -> do
      let t = T.pack expr
      let env = E.baseEnv
      case P.parse t of
        Left err -> putStrLn (T.unpack err)
        Right ast -> do
          case E.evalProgram env ast of
            Left err -> putStrLn (T.unpack err)
            Right result -> mapM_ print result
    _ -> putStrLn $ "Usage: " ++ progName ++ " <expression>"
