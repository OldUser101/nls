module Util
  ( aToRValue,
    lookupEnv,
    define,
    mapAccumM,
  )
where

import qualified Data.Map as M
import Types

-- directly convert a AST value toa  runtime value
aToRValue :: NlsAstValue -> NlsRunValue
aToRValue (ANumber n) = RNumber n
aToRValue (AString s) = RString s
aToRValue (ASymbol s) = RSymbol s
aToRValue (AList xs) = RList (map aToRValue xs)

-- lookup a key recursively in an environment
lookupEnv :: String -> Env -> Maybe NlsRunValue
lookupEnv key env =
  case M.lookup key (frame env) of
    Just v -> Just v
    Nothing -> parent env >>= lookupEnv key

-- define a binding in the current environment
define :: String -> NlsRunValue -> Env -> Env
define key val env = env {frame = M.insert key val (frame env)}

mapAccumM :: (Monad m) => (acc -> x -> m (y, acc)) -> acc -> [x] -> m ([y], acc)
mapAccumM _ acc [] = pure ([], acc)
mapAccumM f acc (x : xs) = do
  (y, acc') <- f acc x
  (ys, acc'') <- mapAccumM f acc' xs
  pure (y : ys, acc'')
