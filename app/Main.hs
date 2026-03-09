module Main (main) where

import qualified Data.Text as T
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype Error = Error T.Text deriving (Show, Eq, Ord)

type Input = T.Text

instance ShowErrorComponent Error where
  showErrorComponent (Error e) = T.unpack e

type NlsParser a = Parsec Error Input a

data NlsValue
  = Atom String
  | Number Integer
  | String String
  | List [NlsValue]
  deriving (Show, Eq, Ord)

sc :: NlsParser ()
sc = L.space space1 empty empty

parseAtom :: NlsParser NlsValue
parseAtom = do
  first <- letterChar <|> oneOf ("!$%&|*+-/:<=>?@^_~")
  rest <- many (alphaNumChar <|> oneOf ("!$%&|*+-/:<=>?@^_~"))
  return $ Atom (first : rest)

parseNumber :: NlsParser NlsValue
parseNumber = Number <$> L.decimal

parseString :: NlsParser NlsValue
parseString = String <$> (char '"' >> manyTill L.charLiteral (char '"'))

parseList :: NlsParser NlsValue
parseList = do
  _ <- char '('
  elems <- many (sc *> parseValue)
  sc
  _ <- char ')'
  return $ List elems

parseValue :: NlsParser NlsValue
parseValue = parseNumber <|> parseString <|> parseAtom <|> parseList

parseNls :: Input -> Either T.Text NlsValue
parseNls input =
  case runParser parseValue "" input of
    Left err -> Left $ T.pack $ errorBundlePretty err
    Right val -> Right val

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName

  if (length args) /= 1
    then
      putStrLn $ "Usage: " ++ progName ++ " <expression>"
    else
      let t = T.pack (args !! 0)
       in case parseNls t of
            Left err -> putStrLn (T.unpack err)
            Right val -> print val
