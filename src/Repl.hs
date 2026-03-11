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

-- evaluate and print AST
evalMany :: Env -> [NlsAstValue] -> IO Env
evalMany env [] = pure env
evalMany env (x : xs) =
  case E.eval env x of
    Left err -> do
      putStrLn $ "Error: " ++ T.unpack err
      pure env
    Right (val, env') -> do
      print val
      evalMany env' xs

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
        Right ast -> evalMany env ast
      repl newEnv
