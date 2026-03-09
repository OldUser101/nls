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
makeBuiltin :: (Integer -> Integer -> Integer) -> NlsRunValue -> [NlsRunValue] -> Eval NlsRunValue
makeBuiltin op first rest = do
  f <- expectNumber first
  rs <- mapM expectNumber rest
  pure $ RNumber (foldl op f rs)

builtinAdd :: [NlsRunValue] -> Eval NlsRunValue
builtinAdd xs = case xs of
  [] -> pure $ RNumber 0
  (f : rest) -> makeBuiltin (+) f rest

builtinMul :: [NlsRunValue] -> Eval NlsRunValue
builtinMul xs = case xs of
  [] -> pure $ RNumber 1
  (f : rest) -> makeBuiltin (*) f rest

builtinSub :: [NlsRunValue] -> Eval NlsRunValue
builtinSub xs = case xs of
  [] -> Left "builtinSub requires at least one argument"
  [x] -> RNumber . negate <$> expectNumber x
  (f : rest) -> makeBuiltin (-) f rest

builtinDiv :: [NlsRunValue] -> Eval NlsRunValue
builtinDiv xs = case xs of
  [] -> Left "builtinDiv requires at least one argument"
  [x] -> do
    n <- expectNumber x
    if n == 0 then Left "division by zero" else pure $ RNumber (div 1 n)
  (f : rest) -> do
    rs <- mapM expectNumber rest
    if elem 0 rs then Left "division by zero" else makeBuiltin div f rest

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
    ("/", wrapBuiltin builtinDiv)
  ]
