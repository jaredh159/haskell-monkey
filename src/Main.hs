import System.IO (stdout, hFlush)
import System.Environment (getArgs)

import Lexer
import Parser (parseProgram)
import qualified Ast
import Eval (eval)
import Object

main :: IO ()
main = do
  clear
  scrollTop
  putStrLn $ magenta "Welcome to the Monkey REPL!\n"
  args <- getArgs
  case args of
    ("tokens":_) -> handleLine (mapM_ print . tokens)
    ("ast":"string":_) -> handleLine $ parseThen $ print . Ast.stringify
    ("ast":_) -> handleLine $ parseThen $ mapM_ print
    _ -> handleLine $ parseThen (\prog -> case eval (Ast.ProgNode prog) of
      Left err -> putStrLn $ red $ "Eval ERROR: " ++ err
      Right result -> putStrLn $ green $ show result)

parseThen :: (Ast.Program -> IO ()) -> String -> IO ()
parseThen f line = case parseProgram line of
  Left e -> putStrLn $ "Parser ERROR: " ++ e
  Right stmts -> f stmts

handleLine :: (String -> IO ()) -> IO ()
handleLine f = do
  putStr $ cyan ">> "
  hFlush stdout
  line <- getLine
  f line
  handleLine f

-- terminal utilities

scrollTop :: IO ()
scrollTop = putStr "\ESC[H"
clear :: IO ()
clear = putStr "\ESC[2J"

red :: String -> String
red s = "\x1b[31m" ++ s ++ "\x1b[0m"
green :: String -> String
green s = "\x1b[32m" ++ s ++ "\x1b[0m"
cyan :: String -> String
cyan s = "\x1b[36m" ++ s ++ "\x1b[0m"
magenta :: String -> String
magenta s = "\x1b[35m" ++ s ++ "\x1b[0m"
