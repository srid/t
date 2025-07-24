{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module T.LLM where

import Data.Aeson (FromJSON, decode, parseJSON, withObject, (.:), (.:?))
import Data.FileEmbed (embedStringFile)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Ollama (GenerateOps (..), GenerateResponse (..), defaultGenerateOps, generate)
import T.Types (Priority (..), Todo (..))

-- Embed the Types.hs file content at compile time
typesSource :: Text
typesSource = $(embedStringFile "src/T/Types.hs")

-- Todo parsing result (mirrors the actual Todo type structure)
data ParseResult = ParseResult
  { description :: Text
  , deadline :: Maybe Text -- YYYY-MM-DD format
  , context :: Maybe Text
  , priority :: Text -- "Low", "Medium", or "High"
  }
  deriving stock (Show)

instance FromJSON ParseResult where
  parseJSON = withObject "ParseResult" $ \o ->
    ParseResult
      <$> o .: "description"
      <*> o .:? "deadline"
      <*> o .:? "context"
      <*> o .: "priority"

-- Convert text priority to Priority type
parsePriority :: Text -> Priority
parsePriority = \case
  "High" -> High
  "Medium" -> Medium
  "Low" -> Low
  _ -> Medium

-- Parse date string to Day
parseDate :: Text -> Maybe Day
parseDate dateStr = parseTimeM True defaultTimeLocale "%Y-%m-%d" (toString dateStr)

-- Create Todo from ParseResult
createTodo :: Int -> ParseResult -> Todo
createTodo newId result =
  Todo
    { todoId = newId
    , todoDescription = description result
    , todoDeadline = parseDate =<< deadline result
    , todoContext = context result
    , todoPriority = parsePriority (priority result)
    }

-- System prompt that includes the actual Haskell types from source
systemPrompt :: Text
systemPrompt =
  "You are a JSON parser for todo items. You must parse natural language input into JSON format.\n\
  \\n\
  \Required JSON structure:\n\
  \{\n\
  \  \"description\": \"string\" (required - main task description),\n\
  \  \"deadline\": \"YYYY-MM-DD\" or null (optional - extract dates from phrases like 'tomorrow', 'Friday', etc.),\n\
  \  \"context\": \"string\" or null (optional - extract context like 'work', 'home', 'personal'),\n\
  \  \"priority\": \"Low\" | \"Medium\" | \"High\" (required - infer from urgency words)\n\
  \}\n\
  \\n\
  \Current date context: 2025-01-24 (Friday)\n\
  \\n\
  \Examples:\n\
  \Input: 'Buy milk tomorrow'\n\
  \Output: {\"description\":\"Buy milk\",\"deadline\":\"2025-01-25\",\"context\":null,\"priority\":\"Medium\"}\n\
  \\n\
  \Input: 'Urgent: finish report by Friday for work'\n\
  \Output: {\"description\":\"Finish report\",\"deadline\":\"2025-01-31\",\"context\":\"work\",\"priority\":\"High\"}\n\
  \\n\
  \Input: 'call dentist when I have time'\n\
  \Output: {\"description\":\"Call dentist\",\"deadline\":null,\"context\":null,\"priority\":\"Low\"}\n\
  \\n\
  \CRITICAL: Return ONLY valid JSON, no explanations, no code, no other text whatsoever.\n\
  \\n\
  \Parse this input: "

-- Parse user input using Ollama
parseUserInput :: Text -> IO (Either Text Todo)
parseUserInput userInput = do
  let fullPrompt = systemPrompt <> userInput
      ops =
        defaultGenerateOps
          { modelName = "llama3.2"
          , prompt = fullPrompt
          }

  result <- generate ops
  case result of
    Left errorMsg -> pure $ Left $ "Ollama request failed: " <> toText errorMsg
    Right response -> do
      let jsonText = response_ response
      case decode (encodeUtf8 jsonText) of
        Nothing -> pure $ Left $ "Failed to parse LLM output as JSON: " <> jsonText
        Just parseResult -> do
          -- Generate a dummy ID for now (will be replaced by caller)
          let todo = createTodo 0 parseResult
          pure $ Right todo
