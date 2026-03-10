{-# LANGUAGE OverloadedStrings #-}

module Util
  ( aToRValue,
    rToAValue,
    lookupEnv,
    define,
    mergeFrame,
    mapAccumM,
    extendEnv,
    reduceEnv,
    extractParam,
  )
where

import qualified Data.Map as M
import qualified Data.Text as T
import Types

-- directly convert a AST value to a runtime value
aToRValue :: NlsAstValue -> NlsRunValue
aToRValue (ANumber n) = RNumber n
aToRValue (AString s) = RString s
aToRValue (ASymbol s) = RSymbol s
aToRValue (AList xs) = RList (map aToRValue xs)

-- convert a runtime value to an AST value
rToAValue :: NlsRunValue -> Either T.Text NlsAstValue
rToAValue (RSymbol s) = pure $ ASymbol s
rToAValue (RNumber n) = pure $ ANumber n
rToAValue (RString s) = pure $ AString s
rToAValue (RBool True) = pure $ ASymbol "true"
rToAValue (RBool False) = pure $ ASymbol "false"
rToAValue (RList xs) = AList <$> mapM rToAValue xs
rToAValue (RFunction _) = Left "cannot eval a function"
rToAValue RUnit = Left "cannot eval an empty unit"

-- lookup a key recursively in an environment
lookupEnv :: String -> Env -> Maybe NlsRunValue
lookupEnv key env =
  case M.lookup key (frame env) of
    Just v -> Just v
    Nothing -> parent env >>= lookupEnv key

-- define a binding in the current environment
define :: String -> NlsRunValue -> Env -> Either T.Text Env
define key val env =
  case M.lookup key (frame env) of
    Just _ -> Left ("cannot shadow binding " <> T.pack key <> " in current frame")
    Nothing -> Right env {frame = M.insert key val (frame env)}

-- merge a frame into an environment
mergeFrame :: Frame -> Env -> Env
mergeFrame f env = env {frame = M.union f (frame env)}

mapAccumM :: (Monad m) => (acc -> x -> m (y, acc)) -> acc -> [x] -> m ([y], acc)
mapAccumM _ acc [] = pure ([], acc)
mapAccumM f acc (x : xs) = do
  (y, acc') <- f acc x
  (ys, acc'') <- mapAccumM f acc' xs
  pure (y : ys, acc'')

-- extend environment into a new frame
extendEnv :: Env -> Env
extendEnv env = Env {frame = M.empty, parent = Just env}

-- drop the current frame, returning to old environment
reduceEnv :: Env -> Maybe Env
reduceEnv env = do
  p <- parent env
  pure p

extractParam :: NlsAstValue -> Eval String
extractParam (ASymbol s) = Right s
extractParam _ = Left "lambda parameter is not a symbol"
