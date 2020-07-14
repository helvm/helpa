module HelVM.HelPA.Assemblers.Gherkin where


valueParser :: Parser Value
valueParser =
  nullParser <|>
  boolParser <|>
  numberParser <|>
  stringParser

nullParser :: Parser Value
nullParser =
  (string "null" <|>
  string "NULL" <|>
  string "Null") *> pure ValueNull

boolParser :: Parser Value
boolParser = (trueParser *> pure (ValueBool True)) <|> (falseParser *> pure (ValueBool False))
  where
    trueParser = string "True" <|> string "true" <|> string "TRUE"
    falseParser = string "False" <|> string "false" <|> string "FALSE"

----

numberParser :: Parser Value
numberParser = ValueNumber <$> scientific

----

stringParser :: Parser Value
stringParser = (ValueString . unpack . strip) <$> 
  takeTill (\c -> c == '|' || c == '\n')

----

cellParser = do
  skipWhile nonNewlineSpace
  val <- valueParser
  skipWhile (not . barOrNewline)
  char '|'
  return val

----

exampleLineParser :: Parser [Value]
exampleLineParser = do
  char '|'
  cells <- many cellParser
  char '\n'
  return cells
--  where
--    cellParser = ...

----

exampleTableParser :: Parser ExampleTable
exampleTableParser = do
  string "Examples:"
  consumeLine
  keys <- exampleColumnTitleLineParser
  valueLists <- many exampleLineParser
  return $ ExampleTable keys (map (zip keys) valueLists)

----

nonBrackets :: Parser String
nonBrackets = many (satisfy (\c -> c /= '\n' && c /= '<'))

insideBrackets :: Parser String
insideBrackets = do
  char '<'
  key <- many letter
  char '>'
  return key

----

parseStatementLine :: Text -> Parser Statement
parseStatementLine signal = do
  string signal
  char ' '
  pairs <- many ((,) <$> nonBrackets <*> insideBrackets)
  finalString <- nonBrackets
  let (fullString, keys) = buildStatement pairs finalString
  return $ Statement fullString keys
  where
    buildStatement 
      :: [(String, String)] -> String -> (String, [String])
    buildStatement [] last = (last, [])
    buildStatement ((str, key) : rest) rem =
      let (str', keys) = buildStatement rest rem
      in (str <> "<" <> key <> ">" <> str', key : keys)

----

scenarioParser :: Parser Scenario
scenarioParser = do
  string "Scenario: "
  title <- consumeLine
  statements <- many (parseStatement <* char '\n')
  examples <- (exampleTableParser <|> return (ExampleTable [] []))
  return $ Scenario title statements examples

----

backgroundParser :: Parser Scenario
backgroundParser = do
  string "Background:"
  consumeLine
  statements <- many (parseStatement <* char '\n')
  examples <- (exampleTableParser <|> return (ExampleTable [] []))
  return $ Scenario "Background" statements examples

----
