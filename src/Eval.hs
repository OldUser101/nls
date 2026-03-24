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

mapThunk :: [(String, NlsRunValue)] -> [(String, NlsThunk)]
mapThunk = map (\(name, val) -> (name, Right val))

-- create a new base environment with builtins
baseEnv :: Env
baseEnv = extendEnv (Env (M.fromList $ mapThunk B.builtins) Nothing)

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
      ("undef", unDefFunc),
      ("lambda", lambdaFunc),
      ("if", ifFunc),
      ("eval", evalFunc),
      ("quote", quoteFunc),
      ("get", getFunc),
      ("import", importFunc)
    ]

defineFunc :: Env -> [NlsAstValue] -> Eval (NlsRunValue, Env)
defineFunc env [ASymbol name, expr] = do
  -- keywords and meta-functions cannot be overriden
  case M.lookup name keywords of
    Just _ -> Left ("cannot shadow keyword: " <> T.pack name)
    Nothing -> case M.lookup name metaFuncs of
      Just _ -> Left ("cannot shadow meta-function: " <> T.pack name)
      Nothing ->
        let thunk :: NlsThunk
            thunk = do
              (val, _) <- eval env' expr
              pure val
            env' :: Env
            env' = defineUnchecked name thunk env
         in pureWithEnv (RUnit) env'
defineFunc _ _ = Left "define expects a symbol and expression"

unDefFunc :: Env -> [NlsAstValue] -> Eval (NlsRunValue, Env)
unDefFunc env [ASymbol name] =
  case undefine name env of
    Left err -> Left err
    Right env' -> pureWithEnv (RUnit) env'
unDefFunc _ _ = Left "undef expects a symbol"

lambdaFunc :: Env -> [NlsAstValue] -> Eval (NlsRunValue, Env)
lambdaFunc env [AList params, body] = do
  paramNames <- mapM extractParam params
  let fn = createLambda env paramNames [body]
  pureWithEnv fn env
lambdaFunc _ _ = Left "lambda expects a parameter and body list"

ifFunc :: Env -> [NlsAstValue] -> Eval (NlsRunValue, Env)
ifFunc env [_cond, _then, _else] = do
  (res, env') <- eval env _cond
  case res of
    (RBool True) -> eval env' _then
    (RBool False) -> eval env' _else
    (RList []) -> eval env' _else -- nil evaluates to false
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

getFunc :: Env -> [NlsAstValue] -> Eval (NlsRunValue, Env)
getFunc env [mExpr, AString key] = do
  (mVal, _) <- eval env mExpr
  case mVal of
    RModule env' ->
      case lookupEnv key env' of
        Just thunk -> do
          val <- thunk
          pureWithEnv val env
        Nothing -> Left $ "module does not contain: " <> T.pack key
    _ -> Left "expected module"
getFunc _ _ = Left "get expects a module and a string"

importFunc :: Env -> [NlsAstValue] -> Eval (NlsRunValue, Env)
importFunc _ _ = Left "cannot call raw import"

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
createLambda :: Env -> [String] -> [NlsAstValue] -> NlsRunValue
createLambda env params body = RFunction wrapper
  where
    wrapper :: [NlsRunValue] -> Env -> Eval (NlsRunValue, Env)
    wrapper args callEnv = do
      if length args /= length params
        then
          Left
            ( "expected "
                <> T.pack (show $ length params)
                <> " arguments, got "
                <> T.pack (show $ length args)
            )
        else do
          let newFrame = M.fromList $ mapThunk $ zip params args
          let env' = mergeFrame newFrame env
          (val, _) <- evalProgram env' body
          pureWithEnv val callEnv

-- evaluate a single ast value
eval :: Env -> NlsAstValue -> Eval (NlsRunValue, Env)
eval env (ANumber n) = pureWithEnv (RNumber n) env
eval env (AString s) = pureWithEnv (RString s) env
eval env (ASymbol key) = do
  case M.lookup key keywords of
    Just v -> pureWithEnv v env
    Nothing -> case lookupEnv key env of
      Just thunk -> do
        val <- thunk
        pureWithEnv val env
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

-- evaluate a sequence of ast values
evalProgram :: Env -> [NlsAstValue] -> Eval (NlsRunValue, Env)
evalProgram env [] = pure (RUnit, env)
evalProgram env [x] = eval env x
evalProgram env (x : xs) = do
  (_, env') <- eval env x
  evalProgram env' xs
