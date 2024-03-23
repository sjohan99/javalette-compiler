module Main where

import System.Environment (getArgs)
import System.Exit        (exitFailure, exitSuccess)
import System.IO.Error    (isUserError, ioeGetErrorString)
import System.IO          (hPutStrLn, stderr)

import Javalette.Par      (pProg, myLexer)
import TypeChecker        (typecheck)


main :: IO ()
main = do
  args <- getArgs
  case take 1 args of
    [] -> error "Please provide contents of a javalette source file as the first argument"
    source:_ -> check source


check :: String -> IO ()
check s = do
  case pProg (myLexer s) of
    Left err  -> do
      hPutStrLn stderr $ "ERROR: Syntax error: " ++ err
      exitFailure
    Right tree -> do
      print tree
      case typecheck tree of
        Left err -> do
          hPutStrLn stderr $ "ERROR: Type error: " ++ err
          exitFailure
        Right tree' -> hPutStrLn stderr "OK" >> exitSuccess
