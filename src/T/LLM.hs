{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module T.LLM where

import Data.Aeson (FromJSON, decode, parseJSON, withObject, (.:), (.:?))
import Data.FileEmbed (embedStringFile)
import Data.Time (Day, defaultTimeLocale, formatTime, getCurrentTime, parseTimeM)
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

-- Generate dynamic system prompt with current date and existing contexts
generateSystemPrompt :: Text -> [Text] -> Text
generateSystemPrompt currentDate existingContexts =
  let contextList =
        if null existingContexts
          then "No existing contexts yet"
          else "Existing contexts: " <> intercalate ", " (map toString existingContexts)
   in "You are a JSON parser for todo items. You must parse natural language input into JSON format.\n\
      \\n\
      \Required JSON structure:\n\
      \{\n\
      \  \"description\": \"string\" (required - main task description),\n\
      \  \"deadline\": \"YYYY-MM-DD\" or null (optional - extract dates from phrases like 'tomorrow', 'Friday', '1st of next month', etc.),\n\
      \  \"context\": \"string\" or null (optional - extract context like 'work', 'home', 'personal', or match existing contexts),\n\
      \  \"priority\": \"Low\" | \"Medium\" | \"High\" (required - infer from urgency words)\n\
      \}\n\
      \\n\
      \Current date and time: "
        <> currentDate
        <> "\n\
           \"
        <> toText contextList
        <> "\n\
           \\n\
           \Date parsing guidelines:\n\
           \- Use the current date above for relative dates like 'tomorrow', 'next week', '1st of next month'\n\
           \- For 'next month', add 1 to current month (handle year rollover)\n\
           \- For day-of-month like '1st', '15th', determine if current or next month based on current date\n\
           \\n\
           \Context matching:\n\
           \- Try to match or infer contexts from existing ones when possible\n\
           \- Create new contexts only when clearly different from existing ones\n\
           \\n\
           \Examples:\n\
           \Input: 'Buy milk tomorrow'\n\
           \Output: {\"description\":\"Buy milk\",\"deadline\":\"[tomorrow's date]\",\"context\":null,\"priority\":\"Medium\"}\n\
           \\n\
           \Input: 'Urgent: finish report by Friday for work'\n\
           \Output: {\"description\":\"Finish report\",\"deadline\":\"[Friday's date]\",\"context\":\"work\",\"priority\":\"High\"}\n\
           \\n\
           \CRITICAL: Return ONLY valid JSON, no explanations, no code, no other text whatsoever.\n\
           \\n\
           \Parse this input: "

-- Extract existing contexts from todo list
extractContexts :: [Todo] -> [Text]
extractContexts todos = ordNub $ mapMaybe todoContext todos

-- Parse user input using Ollama with current date and existing contexts
parseUserInput :: [Todo] -> Text -> IO (Either Text Todo)
parseUserInput existingTodos userInput = do
  currentTime <- getCurrentTime
  let currentDateTime = formatTime defaultTimeLocale "%Y-%m-%d (%A) at %H:%M" currentTime
      existingContexts = extractContexts existingTodos
      systemPrompt = generateSystemPrompt (toText currentDateTime) existingContexts
      fullPrompt = systemPrompt <> userInput
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
