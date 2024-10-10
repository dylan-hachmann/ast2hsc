module Main where

import MyLib
import Text.Pretty.Simple
import Control.Monad.Trans.Reader
import qualified Data.Vector as V
import qualified Data.Map as M
import System.Environment (getArgs)

data Env = Env { getTdMap :: M.Map String String
               , getASTNodes :: V.Vector ASTObject
               } deriving Show

main :: IO ()
main = do
  args <- getArgs
  translationUnitDecl <- invokeClang args
  tdMap <- getTypedefsMap (return translationUnitDecl)
  let header = last args
      astNodes = getASTNodesFromFile header translationUnitDecl
      env = Env tdMap astNodes
  putStrLn (show env)
  return ()
