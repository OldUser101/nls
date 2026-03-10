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
baseEnv = extendEnv (Env (M.fromList B.builtins) Nothing)

apply :: NlsRunValue -> [NlsRunValue] -> Env -> Eval (NlsRunValue, Env)
apply (RFunction f) args env = do
  let env' = extendEnv env
  (val, env'') <- f args env'
  case reduceEnv env'' of
    Just oldEnv -> pureWithEnv val oldEnv
    Nothing -> Left "no environment to unwrap"
apply _ _ _ = Left "attempted to call non-function"

-- create a RFunction from a lambda expression
createLambda :: [String] -> [NlsAstValue] -> NlsRunValue
createLambda params body = RFunction wrapper
  where
    wrapper :: [NlsRunValue] -> Env -> Eval (NlsRunValue, Env)
    wrapper args env = do
      if length args /= length params
        then Left ("expected " <> T.pack (show $ length params) <> " arguments, got " <> T.pack (show $ length args))
        else do
          let newFrame = M.fromList $ zip params args
          let env' = mergeFrame newFrame env
          (res, env'') <- evalProgram env' body

          case res of
            [r] -> pureWithEnv r env''
            rs -> pureWithEnv (RList rs) env''

eval :: Env -> NlsAstValue -> Eval (NlsRunValue, Env)
eval env (ANumber n) = pureWithEnv (RNumber n) env
eval env (AString s) = pureWithEnv (RString s) env
eval env (ASymbol "true") = pureWithEnv (RBool True) env
eval env (ASymbol "false") = pureWithEnv (RBool False) env
eval env (ASymbol key) = do
  case lookupEnv key env of
    Just v -> pureWithEnv v env
    Nothing -> Left (T.pack ("undefined symbol: " ++ key))
eval env (AList []) = pureWithEnv (RList []) env
eval env (AList (ASymbol "quote" : xs)) =
  case xs of
    [expr] -> pureWithEnv (aToRValue expr) env
    _ -> Left "quote expects one argument"
eval env (AList (ASymbol "if" : xs)) =
  case xs of
    [_cond, _then, _else] -> do
      (res, env') <- eval env _cond
      case res of
        (RBool True) -> eval env' _then
        (RBool False) -> eval env' _else
        _ -> Left "expected boolean condition"
    _ -> Left "if expects a condition, then, and else"
eval env (AList (ASymbol "lambda" : xs)) =
  case xs of
    [AList params, body] -> do
      paramNames <- mapM extractParam params
      let fn = createLambda paramNames [body]
      pureWithEnv fn env
    _ -> Left "lambda expects a parameter and body list"
eval env (AList (ASymbol "define" : xs)) =
  case xs of
    [ASymbol name, expr] -> do
      (val, env') <- eval env expr
      case define name val env' of
        Left err -> Left err
        Right env'' -> pureWithEnv (RUnit) env''
    _ -> Left "define expects a symbol and expression"
eval env (AList (f : args)) = do
  (func, env') <- eval env f
  (vals, env'') <- mapAccumM eval env' args
  apply func vals env''

evalProgram :: Env -> [NlsAstValue] -> Eval ([NlsRunValue], Env)
evalProgram env [] = pure ([], env)
evalProgram env (x : xs) = do
  (val, env') <- eval env x
  (vals, env'') <- evalProgram env' xs
  pure (val : vals, env'')
