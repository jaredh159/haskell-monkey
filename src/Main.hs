import System.IO (stdout, hFlush)
import System.Environment (getArgs)

import Lexer
import Parser (parseProgram)
import Ast (Program (Program), stringify)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("tokens":_) -> handleLine (mapM_ print . tokens)
    ("ast":"string":_) -> handleLine (parse (print . stringify))
    _ -> handleLine (parse (\(Program stmts) -> mapM_ print stmts))

parse :: (Program -> IO ()) -> String -> IO ()
parse f line = case parseProgram line of
  Left e -> putStrLn $ "Parser ERROR: " ++ e
  Right program -> f program

handleLine :: (String -> IO ()) -> IO ()
handleLine f = do
  putStr ">> "
  hFlush stdout
  line <- getLine
  f line
  handleLine f

