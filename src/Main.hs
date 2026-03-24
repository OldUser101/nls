{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Eval as E
import qualified Parser as P
import System.Environment (getArgs)
import System.IO
  ( hFlush,
    isEOF,
    stdout,
  )
import Types
import qualified Util as U

-- load, parse, and evaluate a file
evalFile :: Env -> FilePath -> IO (Eval (NlsRunValue, Env))
evalFile env path = do
  t <- TIO.readFile path
  case P.parse t of
    Left err -> pure $ Left err
    Right ast -> evalTop env ast

-- evaluate top level ast, handling import
evalTop :: Env -> [NlsAstValue] -> IO (Eval (NlsRunValue, Env))
evalTop env [] = pure $ Right (RUnit, env)
evalTop env [x] =
  case x of
    AList (ASymbol "import" : [AString path, ASymbol name]) -> do
      res <- evalFile E.baseEnv path
      case res of
        Left err -> pure $ Left err
        Right (_, env') ->
          case U.define name (RModule env') env of
            Left err -> pure $ Left err
            Right env'' -> pure $ Right (RUnit, env'')
    AList (ASymbol "import" : _) -> pure $ Left "import expects a file path and symbol"
    _ -> pure $ E.eval env x
evalTop env (x : xs) = do
  res <- evalTop env [x]
  case res of
    Left err -> pure $ Left err
    Right (_, env') -> evalTop env' xs

-- evaluate and print AST
evalMany :: Env -> [NlsAstValue] -> IO Env
evalMany env [] = pure env
evalMany env (x : xs) = do
  res <- evalTop env [x]
  case res of
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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      res <- evalFile E.baseEnv filePath
      case res of
        Left err -> putStrLn (T.unpack err)
        Right (val, _) -> print val
    _ -> repl E.baseEnv
