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

-- create a new base environment with builtins
baseEnv :: Env
baseEnv = extendEnv (Env (M.fromList B.builtins) Nothing)

-- keywords map for predefined values
-- this is separate from builtins, these cannot be shadowed
keywords :: M.Map String NlsRunValue
keywords =
  M.fromList
    [ ("true", RBool True),
      ("false", RBool False),
      ("nil", RList [])
    ]

-- META FUNCTIONS --

-- meta functions, like keywords, cannot be shadowed
metaFuncs :: M.Map String (Env -> [NlsAstValue] -> Eval (NlsRunValue, Env))
metaFuncs =
  M.fromList
    [ ("define", defineFunc),
      ("undefine", unDefineFunc),
      ("lambda", lambdaFunc),
      ("if", ifFunc),
      ("eval", evalFunc),
      ("quote", quoteFunc)
    ]

defineFunc :: Env -> [NlsAstValue] -> Eval (NlsRunValue, Env)
defineFunc env [ASymbol name, expr] = do
  -- keywords and meta-functions cannot be overriden
  case M.lookup name keywords of
    Just _ -> Left ("cannot shadow keyword: " <> T.pack name)
    Nothing -> case M.lookup name metaFuncs of
      Just _ -> Left ("cannot shadow meta-function: " <> T.pack name)
      Nothing -> do
        (val, env') <- eval env expr
        case define name val env' of
          Left err -> Left err
          Right env'' -> pureWithEnv (RUnit) env''
defineFunc _ _ = Left "define expects a symbol and expression"

unDefineFunc :: Env -> [NlsAstValue] -> Eval (NlsRunValue, Env)
unDefineFunc env [ASymbol name] =
  case undefine name env of
    Left err -> Left err
    Right env' -> pureWithEnv (RUnit) env'
unDefineFunc _ _ = Left "undefine expects a symbol"

lambdaFunc :: Env -> [NlsAstValue] -> Eval (NlsRunValue, Env)
lambdaFunc env [AList params, body] = do
  paramNames <- mapM extractParam params
  let fn = createLambda paramNames [body]
  pureWithEnv fn env
lambdaFunc _ _ = Left "lambda expects a parameter and body list"

ifFunc :: Env -> [NlsAstValue] -> Eval (NlsRunValue, Env)
ifFunc env [_cond, _then, _else] = do
  (res, env') <- eval env _cond
  case res of
    (RBool True) -> eval env' _then
    (RBool False) -> eval env' _else
    (RList []) -> eval env' _else  -- nil evaluates to false
    (RList _) -> eval env' _then
    _ -> Left "expected boolean condition"
ifFunc _ _ = Left "if expects a condition, then, and else"

evalFunc :: Env -> [NlsAstValue] -> Eval (NlsRunValue, Env)
evalFunc env [expr] = do
  (result, env') <- eval env expr
  case rToAValue result of
    Right ast -> eval env' ast
    Left err -> Left err
evalFunc _ _ = Left "eval expects one argument"

quoteFunc :: Env -> [NlsAstValue] -> Eval (NlsRunValue, Env)
quoteFunc env [expr] = pureWithEnv (aToRValue expr) env
quoteFunc _ _ = Left "quote expects one argument"

-- EVALUATION --

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
        then
          Left
            ( "expected "
                <> T.pack (show $ length params)
                <> " arguments, got "
                <> T.pack (show $ length args)
            )
        else do
          let newFrame = M.fromList $ zip params args
          let env' = mergeFrame newFrame env
          evalProgram env' body

eval :: Env -> NlsAstValue -> Eval (NlsRunValue, Env)
eval env (ANumber n) = pureWithEnv (RNumber n) env
eval env (AString s) = pureWithEnv (RString s) env
eval env (ASymbol key) = do
  case M.lookup key keywords of
    Just v -> pureWithEnv v env
    Nothing -> case lookupEnv key env of
      Just v -> pureWithEnv v env
      Nothing -> Left (T.pack ("undefined symbol: " ++ key))
eval env (AList []) = pureWithEnv (RList []) env
eval env (AList (f : args)) =
  case f of
    ASymbol name ->
      case M.lookup name metaFuncs of
        Just mf -> mf env args
        Nothing -> do
          (func, env') <- eval env f
          (vals, env'') <- mapAccumM eval env' args
          apply func vals env''
    _ -> do
      (func, env') <- eval env f
      (vals, env'') <- mapAccumM eval env' args
      apply func vals env''

evalProgram :: Env -> [NlsAstValue] -> Eval (NlsRunValue, Env)
evalProgram env [] = pure (RUnit, env)
evalProgram env [x] = eval env x
evalProgram env (x : xs) = do
  (_, env') <- eval env x
  evalProgram env' xs
