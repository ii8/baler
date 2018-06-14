
import System.Environment
import System.Exit

import Control.Monad

import qualified Data.ByteString.Lazy as B

import Schema
import Diag
import Bin

readFrom :: String -> IO String
readFrom "-" = getContents
readFrom f = readFile f

bytesFrom :: FilePath -> IO B.ByteString
bytesFrom "-" = B.getContents
bytesFrom f = B.readFile f

arg :: String -> String -> [String] -> String
arg _ "" [a] = a
arg def switch (s:v:args')
    | switch == s = v
    | otherwise = arg def switch args'
arg def _ _ = def

display :: String -> String
display "-" = "stdin"
display s = s

check :: [String] -> IO ()
check args =
  let input = arg "-" "" args
      incdir = arg "." "-i" args in do
  str <- readFrom input
  r <- schema incdir (display input) str
  case r of
    Left e -> die (show e)
    Right s -> print s

bin2diag :: [String] -> IO ()
bin2diag args =
  let schemaFile = arg "-" "-s" args
      incdir = arg "." "-i" args
      input = arg "-" "" args in do
  when (schemaFile == input) $
       die "cannot read schema and data from the same file, see `help`"
  str <- readFrom schemaFile
  r <- schema incdir (display schemaFile) str
  case r of
    Left e -> die (show e)
    Right s -> do
      bs <- bytesFrom input
      case decode s bs of
        Left e -> die e
        Right d -> print d

diag2bin :: [String] -> IO ()
diag2bin args =
  let input = arg "-" "" args in do
  str <- readFrom input
  case diag (display input) str of
     Left e -> die (show e)
     Right d -> B.putStr $ encode d

help :: IO ()
help = do
    prog <- getProgName
    putStrLn $ "\
\usage: \n\
\  " ++ prog ++ " [-i dir] [file]\n\
\  " ++ prog ++ " encode [file]\n\
\  " ++ prog ++ " decode [-s file] [-i dir] [file]\n\
\  " ++ prog ++ " help\n\
\\n\
\    -s file   file containing the schema\n\
\    -i dir    include search path\n\
\\n\
\file always defaults to stdin"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("encode":args') -> diag2bin args'
    ("decode":args') -> bin2diag args'
    ("help":_) -> help
    ("-h":_) -> help
    ("-help":_) -> help
    ("--help":_) -> help
    args' -> check args'

