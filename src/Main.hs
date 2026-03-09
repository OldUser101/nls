module Main (main) where

import qualified Data.Text as T
import qualified Parser as P
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName

  if (length args) /= 1
    then
      putStrLn $ "Usage: " ++ progName ++ " <expression>"
    else
      let t = T.pack (args !! 0)
       in case P.parseNls t of
            Left err -> putStrLn (T.unpack err)
            Right val -> print val
