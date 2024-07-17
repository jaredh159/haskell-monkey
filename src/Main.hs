import System.IO (stdout, hFlush)
import Lexer

main :: IO ()
main = do
  handleLine

handleLine :: IO ()
handleLine = do
  putStr ">> "
  hFlush stdout
  line <- getLine
  mapM_ print $ tokens line
  handleLine

