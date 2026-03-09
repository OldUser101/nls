{-# LANGUAGE OverloadedStrings #-}

module Parser where

import qualified Data.Text as T
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
sc = L.space space1 lineComment empty

lineComment :: NlsParser ()
lineComment = L.skipLineComment (T.pack "#")

lexeme :: NlsParser a -> NlsParser a
lexeme = L.lexeme sc

symbol :: T.Text -> NlsParser T.Text
symbol = L.symbol sc

parseAtom :: NlsParser NlsValue
parseAtom = do
  first <- letterChar <|> oneOf ("!$%&|*+-/:<=>?@^_~" :: String)
  rest <- many (alphaNumChar <|> oneOf ("!$%&|*+-/:<=>?@^_~" :: String))
  pure $ Atom (first : rest)

parseQuote :: NlsParser NlsValue
parseQuote = do
  _ <- char '\''
  v <- parseValue
  pure $ List [Atom "quote", v]

parseNumber :: NlsParser NlsValue
parseNumber = Number <$> lexeme L.decimal

parseString :: NlsParser NlsValue
parseString = String <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

parseList :: NlsParser NlsValue
parseList = List <$> between (symbol "(") (symbol ")") (many parseValue)

parseValue :: NlsParser NlsValue
parseValue =
  lexeme $
    parseQuote
      <|> parseNumber
      <|> parseString
      <|> parseAtom
      <|> parseList

parseProgram :: NlsParser NlsValue
parseProgram = between sc eof (List <$> many parseValue)

parseNls :: Input -> Either T.Text NlsValue
parseNls input =
  case runParser parseProgram "" input of
    Left err -> Left $ T.pack $ errorBundlePretty err
    Right val -> Right val
