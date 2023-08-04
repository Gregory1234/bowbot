module Language.MySQL.ParserUtils where

import Text.Parsec
import Language.Haskell.TH
import Data.Char
import Text.Parsec.Pos
import Control.Monad.Identity

spaces1 :: Stream s m Char => ParsecT s u m ()
spaces1 = skipMany1 space <?> "white space"

inParens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
inParens a = char '(' *> spaces *> a <* spaces <* char ')'

withSpace :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
withSpace a = a <* spaces

reserved :: Stream s m Char => String -> ParsecT s u m String
reserved s = try (string s <* notFollowedBy (satisfy (\x -> isAlphaNum x || x == '_')))

stringLit :: Stream s m Char => Char -> ParsecT s u m String
stringLit q = flip label "string literal" $ do
  _ <- char q
  strings <- many character
  _ <- char q
  return $ [q] ++ concat strings ++ [q]
    where
      escape = do
        d <- char '\\'
        c <- oneOf $ q:"\\0nrvtbf"
        return [d, c]
      nonEscape = noneOf $ q:"\\\0\n\r\v\t\b\f"
      character = fmap return nonEscape <|> escape

numberLit :: Stream s m Char => ParsecT s u m String
numberLit = flip label "number literal" $ many1 digit

commaParser :: Stream s m Char => ParsecT s u m ()
commaParser = try $ char ',' *> spaces

backQuoteNameLit :: Stream s m Char => ParsecT s u m String
backQuoteNameLit = flip label "back quoted name" $ do
  _ <- char '`'
  str <- many1 $ satisfy (\x -> isAsciiLower x || x == '_')
  _ <- char '`'
  return str

parserEntire :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parserEntire f = do
  spaces
  ret <- f
  spaces
  eof
  return ret

parseFromLoc :: Stream s Identity t => Parsec s () a -> Loc -> s -> Either ParseError a
parseFromLoc parser loc = parse (setPosition (uncurry (newPos (loc_filename loc)) (loc_start loc)) *> parser) (loc_filename loc)