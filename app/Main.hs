module Main where

import Control.Monad.Trans.Reader
import MyLib
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  env <- loadEnv
  let str = runReader renderAll env
  putStrLn str
  return ()

loadEnv :: IO Env
loadEnv = do
  args <- getArgs
  -- Either read from stdin or get the first arg
  fileHandle <- if null args
                then return stdin
                else openFile (head args) ReadMode
  translationUnitDecl <- decodeFromHandle fileHandle
  let header = last $ getFilesInTU translationUnitDecl
  tdMap <- getTypedefsMap (return translationUnitDecl)
  let astNodes = getASTNodesFromFile header translationUnitDecl
  return (Env header tdMap astNodes)
