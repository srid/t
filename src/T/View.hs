{-# LANGUAGE OverloadedStrings #-}

module T.View where

import Data.Time (fromGregorian)
import Htmx.Lucid.Core (hxPost_, hxSwap_, hxTarget_)
import Htmx.Lucid.Extra (hxIndicator_)
import Lucid
import T.Types (Priority (..), Todo (..), formatDeadline)

-- HTML page structure
page :: [Todo] -> Html ()
page todos = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "t - Todo App"
    script_ [src_ "https://unpkg.com/htmx.org@1.9.12"] ("" :: Text)
    script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)
  body_ [class_ "bg-gray-50 min-h-screen"] $ do
    div_ [class_ "max-w-3xl mx-auto p-8"] $ do
      h1_ [class_ "text-3xl font-bold text-gray-900 mb-8 text-center"] "Todo List"
      div_ [id_ "main-content"] $ mainContentView todos

-- Search box component (Google-like input)
searchBoxView :: Html ()
searchBoxView = do
  form_ [hxPost_ "/add-todo", hxTarget_ "#main-content", hxSwap_ "innerHTML", hxIndicator_ "#loading"] $ do
    div_ [class_ "relative"] $ do
      input_
        [ class_ "w-full px-6 py-3 text-lg border-2 border-gray-200 rounded-full outline-none mb-8 shadow-sm focus:border-blue-500 focus:shadow-lg transition-all duration-200 htmx-request:opacity-50 htmx-request:cursor-wait"
        , placeholder_ "Add a new todo..."
        , name_ "description"
        , type_ "text"
        , autofocus_
        ]
      -- Loading indicator
      div_
        [ id_ "loading"
        , class_ "htmx-indicator absolute right-4 top-3 flex items-center gap-2 text-blue-600"
        ]
        $ do
          div_ [class_ "animate-spin rounded-full h-5 w-5 border-b-2 border-blue-600"] ""
          span_ [class_ "text-sm font-medium"] "🧠 Thinking..."

-- Main content area (form + todo list)
mainContentView :: [Todo] -> Html ()
mainContentView todos = do
  searchBoxView
  div_ [id_ "todo-list"] $ todoListView todos

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
    div_ [class_ "flex-1"] $ do
      div_ [class_ "text-gray-900"] $ toHtml (todoDescription todo)
      when (isJust $ todoDeadline todo) $
        div_ [class_ "text-sm text-gray-600 mt-1"] $ do
          span_ [class_ "inline-flex items-center gap-1"] $ do
            span_ "📅"
            toHtml $ formatDeadline (fromMaybe (fromGregorian 2025 1 1) (todoDeadline todo))
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

-- Error view for LLM parsing failures
errorView :: Text -> Html ()
errorView errorMsg =
  div_ [class_ "bg-red-50 border border-red-200 rounded-lg p-4 m-4", id_ "todo-list"] $ do
    div_ [class_ "flex items-center gap-2 text-red-800"] $ do
      span_ [class_ "text-lg"] "⚠️"
      div_ $ do
        div_ [class_ "font-semibold"] "Failed to parse your input"
        div_ [class_ "text-sm text-red-600 mt-1"] $ toHtml errorMsg
        div_
          [class_ "text-xs text-red-500 mt-2"]
          "Try being more specific, like: 'Buy groceries tomorrow for home' or 'Urgent: finish report by Friday'"
