module Repl (repl) where

import qualified Data.Text as T
import qualified Eval as E
import qualified Parser as P
import System.IO
  ( hFlush,
    isEOF,
    stdout,
  )
import Types

repl :: Env -> IO ()
repl env = do
  putStr "nls> "
  hFlush stdout
  eof <- isEOF
  if eof
    then putStrLn "\nGoodbye!"
    else do
      line <- getLine
      let input = T.pack line
      newEnv <- case P.parse input of
        Left err -> do
          putStrLn $ "Parse error: " ++ (T.unpack err)
          pure env
        Right ast -> do
          case E.evalProgram env ast of
            Left err -> do
              putStrLn $ "Error: " ++ (T.unpack err)
              pure env
            Right (results, env') -> do
              mapM_ print results
              pure env'
      repl newEnv
