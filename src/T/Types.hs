{-# LANGUAGE OverloadedStrings #-}

module T.Types where

import Data.Time (Day, defaultTimeLocale, formatTime, fromGregorian)

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

-- Sample todos
sampleTodos :: [Todo]
sampleTodos =
  [ Todo 1 "Buy groceries" (Just $ fromGregorian 2025 1 28) (Just "home") Medium
  , Todo 2 "Finish Haskell project" (Just $ fromGregorian 2025 1 31) (Just "work") High
  , Todo 3 "Call dentist for appointment" Nothing (Just "health") Low
  , Todo 4 "Review pull requests" (Just $ fromGregorian 2025 1 26) (Just "work") Medium
  ]

-- Helper function to format deadline for display
formatDeadline :: Day -> Text
formatDeadline = toText . formatTime defaultTimeLocale "%b %d, %Y"
