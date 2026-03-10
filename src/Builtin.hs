{-# LANGUAGE OverloadedStrings #-}

module Builtin
  ( builtins,
  )
where

import Types

expectNumber :: NlsRunValue -> Eval Integer
expectNumber (RNumber n) = pure n
expectNumber _ = Left "expected number"

-- template for chained arithmetic operations
makeBuiltinArith :: (Integer -> Integer -> Integer) -> NlsRunValue -> [NlsRunValue] -> Eval NlsRunValue
makeBuiltinArith op first rest = do
  f <- expectNumber first
  rs <- mapM expectNumber rest
  pure $ RNumber (foldl op f rs)

-- template for chained comparison
makeBuiltinComp :: (Integer -> Integer -> Bool) -> NlsRunValue -> [NlsRunValue] -> Eval NlsRunValue
makeBuiltinComp op first rest = do
  f <- expectNumber first
  rs <- mapM expectNumber rest
  pure $ RBool (and (zipWith op (f : rs) rs))

builtinAdd :: [NlsRunValue] -> Eval NlsRunValue
builtinAdd xs = case xs of
  [] -> pure $ RNumber 0
  (f : rest) -> makeBuiltinArith (+) f rest

builtinMul :: [NlsRunValue] -> Eval NlsRunValue
builtinMul xs = case xs of
  [] -> pure $ RNumber 1
  (f : rest) -> makeBuiltinArith (*) f rest

builtinSub :: [NlsRunValue] -> Eval NlsRunValue
builtinSub xs = case xs of
  [] -> Left "builtinSub requires at least one argument"
  [x] -> RNumber . negate <$> expectNumber x
  (f : rest) -> makeBuiltinArith (-) f rest

builtinDiv :: [NlsRunValue] -> Eval NlsRunValue
builtinDiv xs = case xs of
  [] -> Left "builtinDiv requires at least one argument"
  [x] -> do
    n <- expectNumber x
    if n == 0 then Left "division by zero" else pure $ RNumber (div 1 n)
  (f : rest) -> do
    rs <- mapM expectNumber rest
    if elem 0 rs then Left "division by zero" else makeBuiltinArith div f rest

builtinEq :: [NlsRunValue] -> Eval NlsRunValue
builtinEq xs = case xs of
  [] -> pure $ RBool True
  (f : rest) -> makeBuiltinComp (==) f rest

builtinNEq :: [NlsRunValue] -> Eval NlsRunValue
builtinNEq xs = case xs of
  [] -> pure $ RBool True
  (f : rest) -> makeBuiltinComp (/=) f rest

builtinGt :: [NlsRunValue] -> Eval NlsRunValue
builtinGt xs = case xs of
  [] -> pure $ RBool True
  (f : rest) -> makeBuiltinComp (>) f rest

builtinLt :: [NlsRunValue] -> Eval NlsRunValue
builtinLt xs = case xs of
  [] -> pure $ RBool True
  (f : rest) -> makeBuiltinComp (<) f rest

builtinGtEq :: [NlsRunValue] -> Eval NlsRunValue
builtinGtEq xs = case xs of
  [] -> pure $ RBool True
  (f : rest) -> makeBuiltinComp (>=) f rest

builtinLtEq :: [NlsRunValue] -> Eval NlsRunValue
builtinLtEq xs = case xs of
  [] -> pure $ RBool True
  (f : rest) -> makeBuiltinComp (<=) f rest

wrapBuiltin :: ([NlsRunValue] -> Eval NlsRunValue) -> NlsRunValue
wrapBuiltin f = RFunction wrapped
  where
    wrapped :: [NlsRunValue] -> Env -> Eval (NlsRunValue, Env)
    wrapped args env = do
      val <- f args
      pure (val, env)

-- builtins "export" table
builtins :: [(String, NlsRunValue)]
builtins =
  [ ("+", wrapBuiltin builtinAdd),
    ("*", wrapBuiltin builtinMul),
    ("-", wrapBuiltin builtinSub),
    ("/", wrapBuiltin builtinDiv),
    ("=", wrapBuiltin builtinEq),
    ("/=", wrapBuiltin builtinNEq),
    (">", wrapBuiltin builtinGt),
    ("<", wrapBuiltin builtinLt),
    (">=", wrapBuiltin builtinGtEq),
    ("<=", wrapBuiltin builtinLtEq)
  ]
