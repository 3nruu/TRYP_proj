module Main (main) where


-- =====================================================================
--   AST
-- =====================================================================

data Value
  = VObj  [(String, Value)]   -- объект
  | VArr  [Value]             -- массив
  | VStr  String              -- строка
  | VNum  Double              -- число
  | VBool Bool                -- true / false
  | VNull                     -- null
  | VLabel String Value       -- ident@value
  | VRef  String              -- @ident
  deriving (Eq, Show)


main :: IO ()
main = return ()
