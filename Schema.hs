
module Schema (schema) where

import Control.Monad
import Control.Monad.Trans

import Data.Maybe
import Data.Char (isDigit)

import Text.Parsec
import Text.Parsec.Pos

import Data.HashMap.Strict as Map
import Data.HashSet as Set

import Type

type Includes = HashSet String
type Bindings = HashMap String ([String], Raw)
type Arguments = HashMap String Raw
type ParseState = (Includes, Bindings)
type Parser = ParsecT String ParseState IO

afst :: (a -> c) -> (a, b) -> (c, b)
afst f (a, b) = (f a, b)

asnd :: (b -> c) -> (a, b) -> (a, c)
asnd f (a, b) = (a, f b)

putInclude :: String -> Parser ()
putInclude s = modifyState (afst (Set.insert s))

putBinding :: String -> ([String], Raw) -> Parser ()
putBinding s p = modifyState (asnd (Map.insert s p))

getIncludes :: Parser Includes
getIncludes = fst <$> getState

fb :: Raw -> Parser (Maybe ([String], Raw))
fb = return . Just . (,) []

fbx :: Raw -> Parser (Maybe ([String], Raw))
fbx = return . Just . (,) ["x"]

findBinding :: String -> Parser (Maybe ([String], Raw))
findBinding "u8" = fb U8
findBinding "u16" = fb U16
findBinding "u32" = fb U32
findBinding "u64" = fb U64
findBinding "i8" = fb I8
findBinding "i16" = fb I16
findBinding "i32" = fb I32
findBinding "i64" = fb I64
findBinding "f32" = fb F32
findBinding "f64" = fb F64
findBinding "uv" = fb UV
findBinding "none" = fb $ UnionR []
findBinding "void" = fb $ TupleR []
findBinding "bool" = fb $
    UnionR [
        (Just "false", TupleR []),
        (Just "true", TupleR [])
    ]
findBinding "maybe" = fbx $
    UnionR [
        (Just "nothing", TupleR []),
        (Just "just", NameR "x")
    ]
findBinding "map" =
    return . Just . (,) ["k", "v"] $
    ArrayR $
        TupleR [
            (Just "key", NameR "k"),
            (Just "value", NameR "v")
        ]
findBinding "string" = fb $ ArrayR U8
findBinding "utf8" = fb $ ArrayR U8
findBinding s = if all isDigit s
    then fbx $ TupleR (replicate (read s) (Nothing, NameR "x"))
    else Map.lookup s . snd <$> getState

comment :: Parser ()
comment = char ';' >> manyTill anyChar newline >> return ()

ignore' :: Parser ()
ignore' = void space <|> comment <?> ""

ignore :: Parser ()
ignore = skipMany ignore'

ignore1 :: Parser ()
ignore1 = skipMany1 ignore'

lexeme :: Parser p -> Parser p
lexeme p = p <* ignore1

word :: Parser String
word = many1 $ alphaNum <|> oneOf "._-<>?!"

annotation :: Parser String
annotation = lexeme (word <* char ':') <?> "an annotation"

keyword :: String -> Parser ()
keyword = void . try . lexeme . string

subst :: Arguments -> Raw -> Raw
subst args r = case r of
    NameR n -> fromJust (Map.lookup n args)
    TupleR t -> TupleR $ fmap subst' t
    UnionR u -> UnionR $ fmap subst' u
    ArrayR a -> ArrayR $ subst args a
    base -> base
  where
    subst' (a, b) = (a, subst args b)

typep :: [String] -> Parser Raw
typep params = choice [
    keyword "tuple" >> TupleR <$> block,
    keyword "union" >> UnionR <$> block,
    keyword "array" >> ArrayR <$> typep params,
    flip label "a type name" $ do
      n <- word
      if elem n params
        then ignore1 >> return (NameR n)
        else beta n
    ]
  where
    block = manyTill noted (keyword "end")
    noted = (,) <$> optionMaybe (try $ annotation) <*> typep params

    beta n = do
      b <- findBinding n
      case b of
        Nothing -> fail $ "could not find binding of " ++ n
        Just (params', raw) -> do
          ignore1
          args <- collect params' Map.empty
          return $ subst args raw

    collect [] m = return m
    collect (p:ps) m = do
      arg <- typep params
      collect ps (Map.insert p arg m)

binding :: Parser ()
binding = do
    keyword "let"
    w <- word
    b <- findBinding w
    when (isJust b) $
         fail $ "binding " ++ w ++ " already exists"
    ignore1
    p <- manyTill name (keyword "be")
    t <- typep p
    putBinding w (p, t)
  where
    name = lexeme word <?> "a parameter"

include :: String -> Parser ()
include path = do
    keyword "include"
    f <- quoted
    is <- getIncludes
    when (Set.member f is) $
         fail "Recusive or duplicate file inclusion detected"
    putInclude f

    oldInput <- getInput
    oldPos <- getPosition

    setInput =<< (lift $ readFile (filepath f))
    setPosition $ initialPos f
    context path >> eof

    setInput oldInput
    setPosition oldPos
    ignore1
  where
    quoted = char '"'
          >> many (noneOf "\"\n")
          <* char '"' <?> "a quoted filename"
    filepath fn = path ++ "/" ++ fn

context :: String -> Parser ()
context path = ignore >> many (include path) >> many binding >> return ()

schema' :: String -> Parser Raw
schema' path = context path >> typep [] <* eof

schema :: String -> SourceName -> String -> IO (Either ParseError Raw)
schema path name input =
    runParserT
        (schema' path)
        (Set.empty, Map.empty)
        name
        input

