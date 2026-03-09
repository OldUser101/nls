module Types
  ( NlsAstValue (..),
    NlsRunValue (..),
    Env (..),
    Eval,
    Frame,
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

type Frame = M.Map String NlsRunValue

data Env = Env
  { frame :: Frame,
    parent :: Maybe Env
  }

type Eval a = Either T.Text a

data NlsRunValue
  = RNumber Integer
  | RString String
  | RSymbol String
  | RList [NlsRunValue]
  | RFunction ([NlsRunValue] -> Env -> Eval (NlsRunValue, Env))
  | RUnit -- an empty value

instance Show NlsRunValue where
  show (RNumber n) = show n
  show (RString s) = show s
  show (RSymbol s) = s
  show (RList xs) = "(" ++ unwords (map show xs) ++ ")"
  show (RFunction _) = "<function>"
  show RUnit = "<empty>"
