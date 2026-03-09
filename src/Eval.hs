{-# LANGUAGE OverloadedStrings #-}

module Eval where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Parser as P

data NlsRunValue
  = RNumber Integer
  | RString String
  | RSymbol String
  | RList [NlsRunValue]
  | RFunction ([NlsRunValue] -> Eval NlsRunValue)

instance Show NlsRunValue where
  show (RNumber n) = show n
  show (RString s) = show s
  show (RSymbol s) = s
  show (RList xs) = "(" ++ unwords (map show xs) ++ ")"
  show (RFunction _) = show ("<function>" :: String)

type Env = M.Map String NlsRunValue

type Eval a = Either T.Text a

expectNumber :: NlsRunValue -> Eval Integer
expectNumber (RNumber n) = pure n
expectNumber _ = Left "expected number"

builtinPlus :: [NlsRunValue] -> Eval NlsRunValue
builtinPlus xs = RNumber . sum <$> mapM expectNumber xs

baseEnv :: Env
baseEnv =
  M.fromList
    [ ("add", RFunction builtinPlus)
    ]

apply :: NlsRunValue -> [NlsRunValue] -> Eval NlsRunValue
apply (RFunction f) args = f args
apply _ _ = Left (T.pack "attempted to call non-function")

eval :: Env -> P.NlsAstValue -> Eval NlsRunValue
eval _ (P.ANumber n) = pure (RNumber n)
eval _ (P.AString s) = pure (RString s)
eval env (P.ASymbol x) = do
  case M.lookup x env of
    Just v -> pure v
    Nothing -> Left (T.pack ("undefined symbol: " ++ x))
eval _ (P.AList []) = pure (RList [])
eval env (P.AList (f : args)) = do
  func <- eval env f
  argVals <- mapM (eval env) args
  apply func argVals

evalProgram :: Env -> P.NlsAstValue -> Eval [NlsRunValue]
evalProgram env (P.AList exprs) = mapM (eval env) exprs
evalProgram env expr = (: []) <$> eval env expr
