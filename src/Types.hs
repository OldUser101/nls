module Types
  ( NlsAstValue (..),
    NlsRunValue (..),
    Env,
    Eval,
  )
where

import qualified Data.Map as M
import qualified Data.Text as T

data NlsAstValue
  = ASymbol String
  | ANumber Integer
  | AString String
  | AList [NlsAstValue]
  deriving (Show, Eq, Ord)

type Env = M.Map String NlsRunValue

type Eval a = Either T.Text a

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
