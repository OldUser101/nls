module Repl (repl) where

import qualified Data.Text as T
import qualified Eval as E
import qualified Parser as P
import System.IO
  ( hFlush,
    isEOF,
    stdout,
  )
import Types

repl :: Env -> IO ()
repl env = do
  putStr "nls> "
  hFlush stdout
  eof <- isEOF
  if eof
    then putStrLn "\nGoodbye!"
    else do
      line <- getLine
      let input = T.pack line
      case P.parse input of
        Left err -> putStrLn $ "Parse error: " ++ (T.unpack err)
        Right ast ->
          case E.evalProgram env ast of
            Left err -> putStrLn $ "Error: " ++ (T.unpack err)
            Right results -> mapM_ print results
      repl env
