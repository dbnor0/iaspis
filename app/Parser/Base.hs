{-# LANGUAGE OverloadedStrings #-}

module Parser.Base where

import Control.Monad (void)
import Data.Text
import Parser.Types (Parser)
import Text.Megaparsec (chunk, between, choice, MonadParsec (try), (<|>), many)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer (skipLineComment, skipBlockComment, space, lexeme)


-- combinators

backtrack :: [Parser a] -> Parser a
backtrack = choice . (<$>) try

endsIn :: Text -> Parser a -> Parser a
endsIn t = flip (<*) (reserved t)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x = do
      f <- op
      y <- p
      rest (f x y)
      <|> return x

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

-- whitespace

lineComment :: Parser ()
lineComment = skipLineComment "//"

blockComment :: Parser ()
blockComment = skipBlockComment "/*" "*/"

whitespace :: Parser ()
whitespace = space1

spaceOrComment :: Parser ()
spaceOrComment = space whitespace lineComment blockComment

-- syntax

lexeme' :: Parser a -> Parser a
lexeme' = lexeme spaceOrComment

reserved :: Text -> Parser ()
reserved = void . lexeme' . chunk

-- like `reserved`, but also returns a syntax element
reserved' :: Text -> a -> Parser a
reserved' k = (<$ reserved k)

semicolon :: Parser ()
semicolon = reserved ";"

comma :: Parser ()
comma = reserved ","

parens :: Parser a -> Parser a
parens = between (reserved "(") (reserved ")")

brackets :: Parser a -> Parser a
brackets = between (reserved "[") (reserved "]")

braces :: Parser a -> Parser a
braces = between (reserved "{") (reserved "}")

block :: Parser a -> Parser a
block = between (reserved "{") (reserved "}")
