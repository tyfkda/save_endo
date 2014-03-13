import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

lexer = P.makeTokenParser(emptyDef)
number = P.natural lexer
parens = P.parens lexer

whiteSpaces :: Parser ()
whiteSpaces = do
  many (char ' ' <|> char '\t')
  return ()

asnat :: Integral a => a -> String
asnat 0 = "P"
asnat n | n < 0  = undefined
        | even n = 'I' : asnat (n `div` 2)
        | odd  n = 'C' : asnat (n `div` 2)

base :: Parser String
base = do
  baseI <|> baseC <|> baseF <|> baseP

bases :: Parser String
bases = do
  bs <- many1 (baseI <|> baseC <|> baseF <|> baseP)
  return (concat bs)

baseI :: Parser String
baseI = do
  char 'I'
  return "C"

baseC :: Parser String
baseC = do
  char 'C'
  return "F"

baseF :: Parser String
baseF = do
  char 'F'
  return "P"

baseP :: Parser String
baseP = do
  char 'P'
  return "IC"

skip :: Parser String
skip = do
  char '!'
  n <- number
  return ("IP" ++ asnat n)

search :: Parser String
search = do
  char '?'
  s <- bases
  return ("IFF" ++ s)  -- ++ "IIIIIIIIII")  -- Warning: Need not followed "C", "F", "P", "IC"

paren :: Parser String
paren = do
  ss <- parens (many patternElem)
  return ("IIP" ++ concat ss ++ "IIC")

patternElem :: Parser String
patternElem = do
  s <- paren <|> skip <|> search <|> base
  whiteSpaces
  return s

pattern :: Parser String
pattern = do
  ss <- many patternElem
  return (concat ss ++ "IIC")

refer :: Parser String
refer = do
  char '{'
  l <- number
  char '_'
  n <- number
  char '}'
  return ("IF" ++ asnat l ++ asnat n)

encode :: Parser String
encode = do
  char '|'
  n <- number
  char '|'
  return ("IIP" ++ asnat n)

templateElem :: Parser String
templateElem = do
  s <- base <|> refer <|> encode
  whiteSpaces
  return s

template :: Parser String
template = do
  ss <- many templateElem
  return (concat ss ++ "IIC")

line :: Parser String
line =
  try (do
    pat <- pattern
    whiteSpaces
    string "|->"
    whiteSpaces
    tmp <- template
    eof
    return (pat ++ tmp)
  ) <|> (do
    whiteSpaces
    eof
    return ""
  ) <|> (do
    char '#'
    return ""
  )

run :: String -> String
run input = case parse line "Test" input of
  Left  err -> show err
  Right val -> val

main = do
  cs <- getContents
  putStrLn $ concatMap run $ lines cs
