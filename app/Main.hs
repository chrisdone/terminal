{-# LANGUAGE LambdaCase #-}

import Language.Haskell.Interpreter
import System.Environment
import Data.List

main :: IO (Either InterpreterError ())
main = do
  args <- getArgs
  case args of
    [fp] ->
      runInterpreter
        (do loadModules [fp]
            ms <- getLoadedModules
            setTopLevelModules ms
            mapM_
              (\md -> do
                 exports <- getModuleExports md
                 case find (== Fun "start") exports of
                   Nothing ->
                     error "Please define a function: start :: Terminal ()"
                   Just {} -> do
                     ty <- typeOf "start"
                     case ty of
                       "Terminal ()" -> do
                         interpreterOk <-
                           typeChecks ("let " ++ interpreter ++ " in interpret")
                         if interpreterOk
                           then do
                             runStmt ("let " ++ interpreter)
                             runStmt "interpret start"
                           else error
                                  "Wasn't able to use your definition of Terminal."
                       _ ->
                         error
                           "The start function doesn't have the right type, should be: Terminal ()")
              ms)
    _ -> error "Expected: MODULE"
  where
    interpreter =
      "interpret t = case t of \
      \Return a -> return a; \
      \Print str next -> do {putStrLn str; interpret next}; \
      \GetLine f -> do {line <- getLine; interpret (f line)}"
