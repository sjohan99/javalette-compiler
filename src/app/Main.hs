module Main where

import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO.Error    (isUserError, ioeGetErrorString)

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
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "syntax OK"
      case typecheck tree of
        Left err -> do
          putStrLn "TYPE ERROR"
          putStrLn err
          exitFailure
        Right tree' -> putStrLn "typecheck OK"
