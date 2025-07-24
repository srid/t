{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (lookup, maximum)
import Data.Time (Day)
import Htmx.Lucid.Core (hxPost_, hxSwap_, hxTarget_)
import Lucid
import Main.Utf8 qualified as Utf8
import Network.HTTP.Types (status200)
import Network.HTTP.Types.URI (parseQuery)
import Network.Wai (Application, lazyRequestBody, pathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (run)

-- Todo Data Types
data Priority = Low | Medium | High
  deriving stock (Show, Eq, Ord)

data Todo = Todo
  { todoId :: Int
  , todoDescription :: Text
  , todoDeadline :: Maybe Day
  , todoContext :: Maybe Text
  , todoPriority :: Priority
  }
  deriving stock (Show, Eq)

-- Global state for todos (in a real app, you'd use a database)
type TodoStore = IORef [Todo]

-- Sample todos
sampleTodos :: [Todo]
sampleTodos =
  [ Todo 1 "Buy groceries" Nothing (Just "home") Medium
  , Todo 2 "Finish Haskell project" Nothing (Just "work") High
  , Todo 3 "Call dentist for appointment" Nothing (Just "health") Low
  , Todo 4 "Review pull requests" Nothing (Just "work") Medium
  ]

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
          description = case lookup "description" formData of
            Just (Just desc) -> decodeUtf8 desc
            _ -> "Untitled todo"
      todos <- readIORef todoStore
      let newId = case todos of
            [] -> 1
            _ -> maximum (map todoId todos) + 1
          newTodo = Todo newId description Nothing Nothing Medium
      writeIORef todoStore (todos ++ [newTodo])
      updatedTodos <- readIORef todoStore
      respond $
        responseLBS status200 [("Content-Type", "text/html")] $
          renderBS $
            todoListView updatedTodos
    _ -> do
      todos <- readIORef todoStore
      respond $
        responseLBS status200 [("Content-Type", "text/html")] $
          renderBS $
            page todos

-- HTML page structure
page :: [Todo] -> Html ()
page todos = doctypehtml_ $ do
  head_ $ do
    title_ "t - Todo App"
    script_ [src_ "https://unpkg.com/htmx.org@1.9.12"] ("" :: Text)
    script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)
  body_ [class_ "bg-gray-50 min-h-screen"] $ do
    div_ [class_ "max-w-3xl mx-auto p-8"] $ do
      h1_ [class_ "text-3xl font-bold text-gray-900 mb-8 text-center"] "Todo List"
      searchBoxView
      div_ [id_ "todo-list"] $ todoListView todos

-- Search box component (Google-like input)
searchBoxView :: Html ()
searchBoxView = do
  form_ [hxPost_ "/add-todo", hxTarget_ "#todo-list", hxSwap_ "outerHTML"] $ do
    input_
      [ class_ "w-full px-6 py-3 text-lg border-2 border-gray-200 rounded-full outline-none mb-8 shadow-sm focus:border-blue-500 focus:shadow-lg transition-all duration-200"
      , placeholder_ "Add a new todo..."
      , name_ "description"
      , type_ "text"
      , autofocus_
      ]

-- Todo list view component
todoListView :: [Todo] -> Html ()
todoListView todos =
  div_ [class_ "bg-white rounded-lg shadow-md", id_ "todo-list"] $ do
    if null todos
      then
        div_ [class_ "p-6 text-center text-gray-500"] $
          span_ "No todos yet. Add one above!"
      else mapM_ todoItemView todos

-- Individual todo item view
todoItemView :: Todo -> Html ()
todoItemView todo = do
  div_ [class_ "flex items-center gap-3 p-4 border-b border-gray-100 last:border-b-0"] $ do
    span_ [class_ "flex-1 text-gray-900"] $ toHtml (todoDescription todo)
    when (isJust $ todoContext todo) $
      span_ [class_ "text-sm text-gray-500"] $
        toHtml $
          "@" <> fromMaybe "" (todoContext todo)
    span_ [class_ $ "px-2 py-1 text-xs font-medium rounded-full " <> priorityClass (todoPriority todo)] $
      toHtml (show (todoPriority todo) :: Text)
  where
    priorityClass = \case
      High -> "bg-red-100 text-red-800"
      Medium -> "bg-yellow-100 text-yellow-800"
      Low -> "bg-green-100 text-green-800"

{- |
 Main entry point.

 `just run` will invoke this function.
-}
main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  Utf8.withUtf8 $ do
    -- Create initial todo store with sample data
    todoStore <- newIORef sampleTodos
    putStrLn "Starting server: http://localhost:3000"
    run 3000 (app todoStore)
