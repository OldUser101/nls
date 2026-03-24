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

-- split a string with character delimiter
splitOn :: Char -> String -> [String]
splitOn c str =
  case break (== c) str of
    (before, []) -> [before]
    (before, _ : after) -> before : splitOn c after

-- make `get` ast
makeGet :: NlsAstValue -> String -> NlsAstValue
makeGet acc key = AList [ASymbol "get", acc, AString key]

-- desugar dotted expr into `get` sequence
desugarDotted :: String -> NlsAstValue
desugarDotted s =
  case splitOn '.' s of
    [] -> AString s -- this should never happen
    (x : xs) -> foldl makeGet (ASymbol x) xs

parseSymbol :: NlsParser NlsAstValue
parseSymbol = do
  first <- letterChar <|> oneOf ("!$%&|*+-/:<=>?@^_~" :: String)
  rest <- many (alphaNumChar <|> oneOf ("!$%&|*+-/:<=>?@^_~." :: String))
  let sym = first : rest
  if elem '.' sym
    then pure $ desugarDotted sym
    else pure $ ASymbol sym

parseQuote :: NlsParser NlsAstValue
parseQuote = do
  _ <- char '\''
  v <- parseValue
  pure $ AList [ASymbol "quote", v]

parseEval :: NlsParser NlsAstValue
parseEval = do
  _ <- char '!'
  v <- parseValue
  pure $ AList [ASymbol "eval", v]

parseNumber :: NlsParser NlsAstValue
parseNumber = ANumber <$> lexeme L.decimal

parseString :: NlsParser NlsAstValue
parseString = AString <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

parseList :: NlsParser NlsAstValue
parseList = AList <$> between (symbol "(") (symbol ")") (many parseValue)

parseValue :: NlsParser NlsAstValue
parseValue =
  lexeme $
    parseEval
      <|> parseQuote
      <|> parseNumber
      <|> parseString
      <|> parseSymbol
      <|> parseList

parseProgram :: NlsParser [NlsAstValue]
parseProgram = between sc eof (many parseValue)

parse :: Input -> Either T.Text [NlsAstValue]
parse input =
  case runParser parseProgram "" input of
    Left err -> Left $ T.pack $ errorBundlePretty err
    Right val -> Right val
