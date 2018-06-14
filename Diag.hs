
module Diag (diag) where

import Data.Word
import Codec.Binary.UTF8.String as UTF8

import Text.Parsec
import Text.Parsec.String

import Type

valid :: (Num a) => (Int -> Integer -> Bool) -> String -> Int -> String -> Parser a
valid out kind bits s =
    let n = read s in
    if out bits n
        then fail $ concat [ s,
            " does not fit in ", kind,
            " number of ", show bits,
            " bits"
            ]
        else return $ fromIntegral n

tonat :: (Num a) => Int -> String -> Parser a
tonat = valid uout "an unsigned"

toint :: (Num a) => Int -> String -> Parser a
toint = valid iout "a signed"

uout :: Int -> Integer -> Bool
uout bits n = n >= 2 ^ bits || n < 0

iout :: Int -> Integer -> Bool
iout bits n = n >= 2 ^ (bits-1) || n < - (2 ^ (bits-1))

size :: Bool -> String -> Parser TypeV
size validInt n = char '\'' >> if validInt
    then unsigned <|> signed <|> floating
    else floating
  where
    unsigned = char 'u' >> choice [
        char '8' >> U8v <$> tonat 8 n,
        string "16" >> U16v <$> tonat 16 n,
        string "32" >> U32v <$> tonat 32 n,
        string "64" >> U64v <$> tonat 64 n
        ]
    signed = char 'i' >> choice [
        char '8' >> I8v <$> toint 8 n,
        string "16" >> I16v <$> toint 16 n,
        string "32" >> I32v <$> toint 32 n,
        string "64" >> I64v <$> toint 64 n
        ]
    floating = char 'f' >> choice [
        string "32" >> return (F32v (read n)),
        string "64" >> return (F64v (read n))
        ]

num :: Parser TypeV
num = do
    i <- integer <?> "a number"
    s <- suffix
    case s of
      "" -> size True i <|> (UVv <$> tonat 64 i)
      s' -> let f = i ++ s' in (size False f)
  where
    (<:>) a b = (:) <$> a <*> b
    digits = many1 digit
    plus = char '+' >> digits
    minus = char '-' <:> digits
    integer = plus <|> minus <|> digits
    suffix = (++) <$> d <*> e
    d = option "" $ char '.' <:> digits
    e = option "" $ oneOf "eE" <:> integer

ws :: Parser ()
ws = spaces <?> ""

annotation :: Parser (Maybe String)
annotation = optionMaybe (try note) <?> "an annotation"
  where
    word = many1 $ alphaNum <|> oneOf "._-<>?!"
    note = word <* char ':' <* ws

tuple :: Parser TypeV
tuple = Tuplev <$> (open >> many pair <* close)
  where
    pair = (,) <$> annotation <*> diag'
    open = char '{' <* ws
    close = char '}' <* ws

union :: Word64 -> Parser TypeV
union n = char '@' >> ws >> Unionv Nothing n <$> diag'

unionum :: Parser TypeV
unionum = do
    n <- num <* ws
    case n of
      UVv n' -> union n' <|> return (UVv n')
      _ -> return n

array :: Parser TypeV
array = Arrayv <$> (open >> many diag' <* close)
  where
    open = char '[' <* ws
    close = char ']' <* ws

str :: Parser TypeV
str = Arrayv . f <$> str'
  where
    open = char '"'
    close = char '"' <* ws
    str' = open >> many (noneOf "\"\n") <* close
    f s = U8v <$> UTF8.encode s

diag' :: Parser TypeV
diag' = choice [ unionum, tuple, array, str ]

diag :: String -> String -> Either ParseError TypeV
diag name input = parse (ws >> diag' <* eof) name input
