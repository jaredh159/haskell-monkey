import System.IO (stdout, hFlush)
import System.Environment (getArgs)

import Lexer
import Parser (parseProgram)
import qualified Ast
import Eval (eval)
import Object

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("tokens":_) -> handleLine (mapM_ print . tokens)
    ("ast":"string":_) -> handleLine $ parseThen $ print . Ast.stringify
    ("ast":_) -> handleLine $ parseThen $ mapM_ print
    -- _ -> handleLine (parseThen (\stmts -> print (eval (ProgNode stmts))))
    _ -> handleLine $ parseThen $ print . eval . Ast.ProgNode
    -- _ -> handleLine (\line -> fmap (\prog -> eval (ProgNode prog)))

parseThen :: (Ast.Program -> IO ()) -> String -> IO ()
parseThen f line = case parseProgram line of
  Left e -> putStrLn $ "Parser ERROR: " ++ e
  Right stmts -> f stmts

handleLine :: (String -> IO ()) -> IO ()
handleLine f = do
  putStr ">> "
  hFlush stdout
  line <- getLine
  f line
  handleLine f

