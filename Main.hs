module Main (main) where

import           Control.Monad         (foldM)
import           Data.List             (intercalate)
import qualified Data.Map.Strict       as Map
import           Data.Map.Strict       (Map)
import           Numeric               (showHex)
import           System.Exit           (ExitCode (..), exitWith)
import           System.IO             (hPutStrLn, hSetEncoding, stderr,
                                        stdin, stdout, utf8)
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

data Err
  = Dup   String      -- ## есил метка определна дважды
  | Undef String      -- ## если не существует 
  | Cycle [String]    -- ## цикл
  deriving Show

-- ## собираем таблицу меток
collect :: Value -> Either Err (Map String Value)
collect = go Map.empty
  where
    go m v = case v of
      VLabel n inner
        | Map.member n m -> Left (Dup n)
        | otherwise      -> go (Map.insert n inner m) inner
      VObj ps -> foldM (\acc (_, x) -> go acc x) m ps
      VArr xs -> foldM go m xs
      _       -> Right m

-- ## убираем Vlabel, заменяем Vref
subst :: Map String Value -> Value -> Either Err Value
subst table = go []
  where
    go path v = case v of
      VLabel _ inner -> go path inner
      VRef n
        | n `elem` path -> Left (Cycle (reverse path ++ [n]))
        | otherwise -> case Map.lookup n table of
            Nothing -> Left (Undef n)
            Just v' -> go (n : path) v'
      VObj ps -> VObj <$> traverse (\(k, x) -> fmap ((,) k) (go path x)) ps
      VArr xs -> VArr <$> traverse (go path) xs
      _       -> Right v

resolve :: Value -> Either Err Value
resolve v = collect v >>= flip subst v


emit :: Value -> String
emit = go 0
  where
    indent d = replicate (d * 2) ' '

    go _ (VStr s)      = jsonStr s
    go _ (VNum n)
      | n == fromIntegral (truncate n :: Integer) && abs n < 1e15
                       = show (truncate n :: Integer)
      | otherwise      = show n
    go _ (VBool True)  = "true"
    go _ (VBool False) = "false"
    go _ VNull         = "null"
    go _ (VObj [])     = "{}"
    go d (VObj ps)     = "{\n"
                      ++ intercalate ",\n"
                           [ indent (d + 1) ++ jsonStr k ++ ": " ++ go (d + 1) x
                           | (k, x) <- ps ]
                      ++ "\n" ++ indent d ++ "}"
    go _ (VArr [])     = "[]"
    go d (VArr xs)     = "[\n"
                      ++ intercalate ",\n"
                           [ indent (d + 1) ++ go (d + 1) x | x <- xs ]
                      ++ "\n" ++ indent d ++ "]"
    go _ _             = error "emit: unresolved label/reference"

    jsonStr s = '"' : concatMap escChar s ++ "\""

    escChar '"'  = "\\\""
    escChar '\\' = "\\\\"
    escChar '\n' = "\\n"
    escChar '\t' = "\\t"
    escChar '\r' = "\\r"
    escChar '\b' = "\\b"
    escChar '\f' = "\\f"
    escChar c
      | c < ' '   = "\\u" ++ pad4 (showHex (fromEnum c) "")
      | otherwise = [c]

    pad4 s = replicate (4 - length s) '0' ++ s


main :: IO ()
main = do
  hSetEncoding stdin  utf8
  hSetEncoding stdout utf8
  src <- getContents
  case parseRjson src of
    Left e -> do
      hPutStrLn stderr ("parse error: " ++ show e)
      exitWith (ExitFailure 1)
    Right v -> case resolve v of
      Left e -> do
        hPutStrLn stderr ("resolve error: " ++ show e)
        exitWith (ExitFailure 2)
      Right r -> putStrLn (emit r)
