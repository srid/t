{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (lookup, maximum)
import Lucid (renderBS)
import Main.Utf8 qualified as Utf8
import Network.HTTP.Types (status200)
import Network.HTTP.Types.URI (parseQuery)
import Network.Wai (Application, lazyRequestBody, pathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (run)
import T.LLM (checkOllama, parseUserInput)
import T.Types (Todo (..), sampleTodos, todoId)
import T.View (errorView, mainContentView, page)

-- Global state for todos (in a real app, you'd use a database)
type TodoStore = IORef [Todo]

-- WAI Application
app :: TodoStore -> Application
app todoStore request respond = do
  let path = pathInfo request
      method = requestMethod request
  case (method, path) of
    ("GET", []) -> do
      todos <- readIORef todoStore
      respond $
        responseLBS status200 [("Content-Type", "text/html")] $
          renderBS $
            page todos
    ("POST", ["add-todo"]) -> do
      body <- lazyRequestBody request
      let formData = parseQuery (toStrict body)
          userInput = case lookup "description" formData of
            Just (Just desc) -> decodeUtf8 desc
            _ -> "Untitled todo"

      todos <- readIORef todoStore
      parseResult <- parseUserInput todos userInput
      case parseResult of
        Left errorMsg -> do
          -- Return error message in HTML
          respond $
            responseLBS status200 [("Content-Type", "text/html")] $
              renderBS $
                errorView errorMsg
        Right parsedTodo -> do
          let newId = case todos of
                [] -> 1
                _ -> maximum (map todoId todos) + 1
              newTodo = parsedTodo {todoId = newId}
          writeIORef todoStore (todos ++ [newTodo])
          updatedTodos <- readIORef todoStore
          respond $
            responseLBS status200 [("Content-Type", "text/html")] $
              renderBS $
                mainContentView updatedTodos
    _ -> do
      todos <- readIORef todoStore
      respond $
        responseLBS status200 [("Content-Type", "text/html")] $
          renderBS $
            page todos

{- |
 Main entry point.

 `just run` will invoke this function.
-}
main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  Utf8.withUtf8 $ do
    -- Set line buffering for immediate log output
    hSetBuffering stdout LineBuffering

    -- Check if Ollama is running before starting
    putStrLn "🔍 Checking Ollama connection..."
    ollamaRunning <- checkOllama
    if not ollamaRunning
      then do
        putStrLn "❌ ERROR: Ollama is not running or not accessible at http://127.0.0.1:11434"
        exitFailure
      else do
        putStrLn "✅ Ollama connection confirmed"

        -- Create initial todo store with sample data
        todoStore <- newIORef sampleTodos
        putStrLn "🚀 Starting intelligent todo app server..."
        putStrLn "📝 Web UI: http://localhost:3000"
        putStrLn "🧠 LLM backend: Ollama (llama3.2)"
        putStrLn "✨ Ready for natural language todo input!"
        run 3000 (app todoStore)
