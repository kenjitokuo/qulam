module Main where

import System.Environment (getArgs)

import QuLam.Syntax
import QuLam.Parser

------------------------------------------------------------
-- 表示用の補助
------------------------------------------------------------

printContext :: Context -> IO ()
printContext [] = putStrLn "  (empty)"
printContext ctx =
  mapM_ (\(x, ty) -> putStrLn ("  " ++ x ++ " : " ++ prettyType ty)) ctx

printTypeError :: TypeError -> IO ()
printTypeError e = putStrLn (prettyTypeError e)

------------------------------------------------------------
-- 何も引数を渡さなかったときの簡単なデモ
------------------------------------------------------------

runExample :: IO ()
runExample = do
  putStrLn "QuLam compiler (stub)"
  let exampleType  = TArr (TVar "a") (TVar "a")
      exampleTerm  = Lam "x" (TVar "a") (Var "x")
  putStrLn $ "exampleType = " ++ prettyType exampleType
  putStrLn $ "exampleTerm = " ++ prettyTerm exampleTerm
  case typeOf [] exampleTerm of
    Left err -> do
      putStrLn "type error in exampleTerm:"
      printTypeError err
    Right ty -> do
      putStrLn $ "typeOf(exampleTerm) = " ++ prettyType ty

------------------------------------------------------------
-- メイン
------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> runExample
    [file] -> runFile file
    _      -> putStrLn "Usage: qulam-compiler [file]"

------------------------------------------------------------
-- ファイルを読み込んで型付け・正規形まで計算
------------------------------------------------------------

runFile :: FilePath -> IO ()
runFile path = do
  src <- readFile path
  putStrLn $ "=== QuLam: " ++ path ++ " ==="
  putStrLn "source:"
  putStrLn src
  putStrLn ""

  case parseFile src of
    Left err -> do
      putStrLn "[parse error]"
      putStrLn err

    Right (ctx, t) -> do
      putStrLn "[context]"
      printContext ctx
      putStrLn "[term]"
      putStrLn ("  " ++ prettyTerm t)
      case typeOf ctx t of
        Left e -> do
          putStrLn "[type error]"
          printTypeError e
        Right ty -> do
          putStrLn "[type]"
          putStrLn ("  " ++ prettyType ty)
          putStrLn "[nf]"
          putStrLn ("  " ++ prettyTerm (nf t))
