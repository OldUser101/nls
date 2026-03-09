{-# LANGUAGE OverloadedStrings #-}

module Eval
  ( baseEnv,
    eval,
    evalProgram,
  )
where

import qualified Builtin as B
import qualified Data.Map as M
import qualified Data.Text as T
import Types

baseEnv :: Env
baseEnv =
  M.fromList
    B.builtins

apply :: NlsRunValue -> [NlsRunValue] -> Eval NlsRunValue
apply (RFunction f) args = f args
apply _ _ = Left (T.pack "attempted to call non-function")

eval :: Env -> NlsAstValue -> Eval NlsRunValue
eval _ (ANumber n) = pure (RNumber n)
eval _ (AString s) = pure (RString s)
eval env (ASymbol x) = do
  case M.lookup x env of
    Just v -> pure v
    Nothing -> Left (T.pack ("undefined symbol: " ++ x))
eval _ (AList []) = pure (RList [])
eval env (AList (f : args)) = do
  func <- eval env f
  argVals <- mapM (eval env) args
  apply func argVals

evalProgram :: Env -> NlsAstValue -> Eval [NlsRunValue]
evalProgram env (AList exprs) = mapM (eval env) exprs
evalProgram env expr = (: []) <$> eval env expr
