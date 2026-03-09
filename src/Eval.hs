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
import Util

pureWithEnv :: NlsRunValue -> Env -> Eval (NlsRunValue, Env)
pureWithEnv val env = pure (val, env)

-- create a new base environment with builtins
baseEnv :: Env
baseEnv = Env (M.fromList B.builtins) Nothing

apply :: NlsRunValue -> [NlsRunValue] -> Env -> Eval (NlsRunValue, Env)
-- TODO: use proper closures for functions
apply (RFunction f) args env = f args env
apply _ _ _ = Left (T.pack "attempted to call non-function")

eval :: Env -> NlsAstValue -> Eval (NlsRunValue, Env)
eval env (ANumber n) = pureWithEnv (RNumber n) env
eval env (AString s) = pureWithEnv (RString s) env
eval env (ASymbol key) = do
  case lookupEnv key env of
    Just v -> pureWithEnv v env
    Nothing -> Left (T.pack ("undefined symbol: " ++ key))
eval env (AList []) = pureWithEnv (RList []) env
eval env (AList (ASymbol "quote" : xs)) =
  case xs of
    [expr] -> pureWithEnv (aToRValue expr) env
    _ -> Left "quote expects one argument"
eval env (AList (ASymbol "define" : xs)) =
  case xs of
    [ASymbol name, expr] -> do
      (val, env') <- eval env expr
      let newEnv = define name val env'
      pureWithEnv (RUnit) newEnv
    _ -> Left "define expects a symbol and expression"
eval env (AList (f : args)) = do
  (func, env') <- eval env f
  argVals <- mapM (eval env') args
  apply func (map fst argVals) env'

evalProgram :: Env -> NlsAstValue -> Eval ([NlsRunValue], Env)
evalProgram env (AList exprs) = mapAccumM eval env exprs
evalProgram env expr = do
  (val, env') <- eval env expr
  pure ([val], env')
