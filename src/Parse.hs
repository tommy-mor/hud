module Parse
  ( block
  , field
  , depexprs
  ) where

import Text.Parsec
import Text.Parsec.String
import Lib
import Debug.Trace
import Data.Maybe (maybe)

comment :: String -> Parser ()
comment s = do
  string s
  _ <- (optionMaybe $ manyTill anyChar (try (newline <|> (eof >> (pure '\n'))))) -- ignore comments
  spaces
  return ()

padding = paddingBase "//"
paddingBase s = try $ do
  spaces
  optional $ sepBy (comment s) spaces

quotedString :: Parser String
quotedString = do
  char '"'
  -- name <- many1 (letter <|> oneOf "/._-" <|> digit)
  name <- many (noneOf "\"\n") -- covers every case except escaped strings
  char '"'
  return name

unquotedString :: Parser String
unquotedString = do
  name <- many (letter <|> (oneOf "_"))-- covers every case except escaped strings
  return name

block :: Parser Node
block = do
  padding
  (quoted, name) <-
    (((,) Quoted) <$> quotedString) <|> (((,) Unquoted) <$> unquotedString)
  padding
  tag <- optionMaybe tagParser
  padding
  inside <-
    between (char '{') (char '}') $
    padding *> (try (many (try ((try field) <|> (try block)))))
  padding
  return $ Block name quoted inside tag

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

-- TODO feature parser
-- TODO test hudlayout.txt parser
-- TODO test parse on hudlayout, where titles don't need to be string quoted
-- Add field to block saying if its quoted or not, just in case it needs to be saved
-- TODO test comments separated in a row

--------------------------------- Feature language parser
data Sexp
  = List String [Sexp]
  | Arg String
  

paddingf = paddingBase "#"

depexprs :: Parser [DepExpr]
depexprs = do
  a <- fmap (map sexpToFeature) $ sepBy fexpr spaces
  eof
  return a

sexpToFeature :: Sexp -> DepExpr
sexpToFeature (List "block-copy" [(Arg filename), (Arg blockname)]) =
  BlockCopy filename blockname
sexpToFeature (List "file-copy" [(Arg filename)]) = FileCopy filename
sexpToFeature (List "animation-copy" [(Arg animationname)]) =
  AnimationCopy animationname


fexpr :: Parser Sexp
fexpr = do
  paddingf
  s <-
    (try (Arg <$> quotedString)) <|>
    (do char '('
        name <- many1 (letter <|> char '-')
        spaces
        args <- sepBy fexpr spaces
        char ')'
        return (List name args))
  paddingf
  return s
