module Lib
  ( someFunc
  , Node (..)
  , block
  , field
  ) where

import Text.Parsec
import Text.Parsec.String

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Node
  = Block String [Node]
  | Field String String (Maybe String)
  deriving (Show, Eq)

comment :: Parser ()
comment = do
  string "//"
  _ <- (optionMaybe $ manyTill anyChar (try newline)) -- ignore comments
  return ()

padding = do
  spaces
  optional comment
  spaces

quotedString :: Parser String
quotedString = do
  char '"'
  name <- many1 (letter <|> oneOf "/._" <|> digit)
  char '"'
  return name

block :: Parser Node
block = do
  padding
  name <- quotedString
  padding
  inside <- between (char '{') (char '}') (many1 ((try field) <|> block))
  padding
  return $ Block name inside

field :: Parser Node
field = do
  padding -- does this cover before and after comments
  name <- quotedString
  padding
  val <- quotedString
  padding
  tag <- optionMaybe tagParser
  padding
  return $ Field name val tag

tagParser :: Parser String
tagParser = do
  spaces
  char '['
  char '$'
  tag <- many1 (oneOf ['A'..'Z'] <|> digit)
  char ']'
  return tag

-- start with thing the recreates every file
-- then change it so it only recreates the needed files
-- then change it so that it splices in the text lines (with a parser that preserves line numbers)
-- and diffs old/new data structures? idk
