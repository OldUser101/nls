{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( Parser.parse,
  )
where

import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types

newtype Error = Error T.Text deriving (Show, Eq, Ord)

type Input = T.Text

instance ShowErrorComponent Error where
  showErrorComponent (Error e) = T.unpack e

type NlsParser a = Parsec Error Input a

sc :: NlsParser ()
sc = L.space space1 lineComment empty

lineComment :: NlsParser ()
lineComment = L.skipLineComment (T.pack "#")

lexeme :: NlsParser a -> NlsParser a
lexeme = L.lexeme sc

symbol :: T.Text -> NlsParser T.Text
symbol = L.symbol sc

parseSymbol :: NlsParser NlsAstValue
parseSymbol = do
  first <- letterChar <|> oneOf ("!$%&|*+-/:<=>?@^_~" :: String)
  rest <- many (alphaNumChar <|> oneOf ("!$%&|*+-/:<=>?@^_~" :: String))
  pure $ ASymbol (first : rest)

parseQuote :: NlsParser NlsAstValue
parseQuote = do
  _ <- char '\''
  v <- parseValue
  pure $ AList [ASymbol "'", v]

parseNumber :: NlsParser NlsAstValue
parseNumber = ANumber <$> lexeme L.decimal

parseString :: NlsParser NlsAstValue
parseString = AString <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

parseList :: NlsParser NlsAstValue
parseList = AList <$> between (symbol "(") (symbol ")") (many parseValue)

parseValue :: NlsParser NlsAstValue
parseValue =
  lexeme $
    parseQuote
      <|> parseNumber
      <|> parseString
      <|> parseSymbol
      <|> parseList

parseProgram :: NlsParser NlsAstValue
parseProgram = between sc eof (AList <$> many parseValue)

parse :: Input -> Either T.Text NlsAstValue
parse input =
  case runParser parseProgram "" input of
    Left err -> Left $ T.pack $ errorBundlePretty err
    Right val -> Right val
