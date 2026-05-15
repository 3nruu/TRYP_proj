module Main (main) where

import           Text.Parsec
import           Text.Parsec.String    (Parser)


data Value
  = VObj  [(String, Value)]                           -- ## объект
  | VArr  [Value]                                     -- ## массив
  | VStr  String
  | VNum  Double
  | VBool Bool
  | VNull
  | VLabel String Value                               -- ## name@value
  | VRef  String                                      -- ## @name
  deriving (Eq, Show)


ws :: Parser ()
ws = skipMany (oneOf " \t\r\n")                       -- ## пропускаем пробелы

lex_ :: Parser a -> Parser a
lex_ p = p <* ws                                      -- ## съедаем пробелы после токена

sym :: String -> Parser String
sym s = lex_ (string s)

ident :: Parser String
ident = lex_ ((:) <$> (letter <|> char '_')
                  <*> many (alphaNum <|> char '_'))   -- ## имя без кавычек

strLit :: Parser String
strLit = lex_ $ between (char '"') (char '"') (many ch)
  where
    ch  = (char '\\' *> esc) <|> noneOf "\"\\"
    esc = choice [ '"'  <$ char '"'
                 , '\\' <$ char '\\'
                 , '/'  <$ char '/'
                 , '\n' <$ char 'n'
                 , '\t' <$ char 't'
                 , '\r' <$ char 'r'
                 , '\b' <$ char 'b'
                 , '\f' <$ char 'f'
                 ]

numLit :: Parser Double
numLit = lex_ $ do
  s <- option ""  (string "-")
  i <- many1 digit
  f <- option ""  ((:) <$> char '.' <*> many1 digit)
  e <- option ""  $ (\c t d -> c : t ++ d)
                       <$> oneOf "eE"
                       <*> option "" (string "+" <|> string "-")
                       <*> many1 digit
  return (read (s ++ i ++ f ++ e))                    -- ## собираем число из частей


value :: Parser Value
value = ws *> choice                                  -- ## пробуем варианты по порядку
  [ identStart
  , VRef <$> (char '@' *> ident)
  , VObj <$> between (sym "{") (sym "}") (pair `sepBy` sym ",")
  , VArr <$> between (sym "[") (sym "]") (value `sepBy` sym ",")
  , VStr <$> strLit
  , VNum <$> numLit
  ]


identStart :: Parser Value
identStart = do
  n <- ident
  (sym "@" *> (VLabel n <$> value)) <|> kw n          -- ## либо метка, либо keyword
  where
    kw "true"  = return (VBool True)
    kw "false" = return (VBool False)
    kw "null"  = return VNull
    kw other   = fail ("unknown bare word: " ++ other ++ " (use \"" ++ other ++ "\")")


pair :: Parser (String, Value)
pair = do
  k <- strLit <|> ident                               -- ## ключ может быть строкой или именем
  _ <- sym ":"
  v <- value
  return (k, v)


parseRjson :: String -> Either ParseError Value
parseRjson = parse (ws *> value <* eof) "<input>"     -- ## не оставляем мусор после значения


main :: IO ()
main = do
  src <- getContents
  case parseRjson src of
    Left  e -> putStrLn ("parse error: " ++ show e)
    Right v -> print v
