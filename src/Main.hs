import System.IO (stdout, hFlush)
import System.Environment (getArgs)
import Control.Monad (void, when)
import Data.List (intercalate)
import qualified Data.Map as M

import Lexer
import Parser (parseProgram)
import qualified Ast
import Eval (evalS)
import qualified Env
import Env (Env, Object(..), objType)

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
    _ -> void $ evalLine Env.empty

parseThen :: (Ast.Program -> IO ()) -> String -> IO ()
parseThen f line = case parseProgram line of
  Left e -> putStrLn $ "Parser ERROR: " ++ e
  Right stmts -> f stmts

evalLine :: Env -> IO Env
evalLine env = do
  putStr $ cyan ">> "
  hFlush stdout
  line <- getLine
  case line of
    ".exit" -> do
      putStrLn $ magenta "Goodbye!"
      pure env
    _ -> case parseProgram line of
      Left e -> do
        putStrLn $ red $ "Parser ERROR: " ++ e
        evalLine env
      Right prog -> case evalS (Ast.ProgNode prog) env of
        (Left err, env') -> do
          putStrLn $ red $ "Eval ERROR: " ++ err
          evalLine env'
        (Right obj, env') -> do
          putStrLn $ showT obj
          evalLine env'

handleLine :: (String -> IO ()) -> IO ()
handleLine f = do
  putStr $ cyan ">> "
  hFlush stdout
  line <- getLine
  f line
  handleLine f

-- terminal utilities

showT :: Object -> String
showT (ObjInt i) = yellow $ show i
showT (ObjBool True) = green "true"
showT (ObjBool False) = red "false"
showT ObjNull = grey "null"
showT (ObjFn {}) = "<fn>"
showT (ObjString string) = cyan $ "\"" ++ string ++ "\""
showT (ObjBuiltIn _) = grey "<builtin fn>"
showT (ObjArray objs) = "[" ++ intercalate ", " (map showT objs) ++ "]"
showT (ObjReturn _) = error "unreachable"
showT (ObjHash hmap) =
  let entries = map (\(key, val) -> showT key ++ ": " ++ showT val) (M.toList hmap) in
  "{" ++ intercalate ", " entries ++ "}"

scrollTop :: IO ()
scrollTop = putStr "\ESC[H"
clear :: IO ()
clear = putStr "\ESC[2J"

grey :: String -> String
grey s = "\x1b[90m" ++ s ++ "\x1b[0m"
yellow :: String -> String
yellow s = "\x1b[33m" ++ s ++ "\x1b[0m"
red :: String -> String
red s = "\x1b[31m" ++ s ++ "\x1b[0m"
green :: String -> String
green s = "\x1b[32m" ++ s ++ "\x1b[0m"
cyan :: String -> String
cyan s = "\x1b[36m" ++ s ++ "\x1b[0m"
magenta :: String -> String
magenta s = "\x1b[35m" ++ s ++ "\x1b[0m"
