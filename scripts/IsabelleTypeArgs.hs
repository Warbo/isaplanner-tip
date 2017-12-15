#!/usr/bin/env runhaskell

import           Control.Applicative
import           Data.Char
import           Data.List                     (intercalate, nub)
import           System.IO
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.String            (Parser)
import           Text.ParserCombinators.Parsec hiding ((<|>))

msg = hPutStrLn stderr

-- Isabelle types
data Type = Chunk String | Group [Type] | Func [Type] Type deriving (Eq)

instance Show Type where
  show t = case t of
      Chunk s        -> s
      Group xs       -> unwords (map wrap xs)
      Func  args ret -> intercalate " => " (map wrap (args ++ [ret]))
    where needParens (Group _)  = True
          needParens (Func _ _) = True
          needParens _          = False
          wrap x = if needParens x
                      then "(" ++ show x ++ ")"
                      else        show x

nonSpecial :: Parser Char
nonSpecial = noneOf "()=>"

chunk :: Parser Type
chunk = fmap Chunk (many1 nonSpecial)

group :: Parser Type
group = fmap Group (between (char '(') (char ')')
                            (many1 parseType))

arrow :: Parser ()
arrow = do
  spaces
  string "=>"
  spaces
  return ()

func :: Parser Type
func = do
  x  <- group <|> chunk
  arrow
  xs <- sepBy1 (group <|> chunk) arrow
  let all = (x:xs)
  return (Func (init all) (last all))

parseType :: Parser Type
parseType = try func <|> group <|> chunk

stringToType :: String -> Type
stringToType s = case parse parseType "(unknown)" ("(" ++ s ++ ")") of
  Left  err -> error ("Couldn't parse '" ++ s ++ "': " ++ show err)
  Right t   -> simplify t

-- Unwrap "groups" with only one member and trim whitespace in chunks
simplify :: Type -> Type
simplify (Group [t]) = simplify t
simplify (Group ts)  = Group (map simplify ts)
simplify (Func xs y) = Func (map simplify xs) (simplify y)
simplify (Chunk s)   = Chunk (trim s)

-- Strip leading/trailing whitespace and replace multiple spaces with one
trim :: String -> String
trim ""                   = ""
trim s | isSpace (head s) = trim (tail s)
trim s | isSpace (last s) = trim (init s)
trim s                    = reverse (go s "")
  where go (x:y:s) acc | isSpace x && isSpace y = go (' ':s)    acc
        go   (c:s) acc = go s (c:acc)
        go ""      acc = acc

argsOf :: Type -> [Type]
argsOf (Func args _) = args ++ concatMap argsOf args
argsOf _             = []

renderArgsOf :: String -> String
renderArgsOf s = unlines (map show (nub (argsOf (stringToType s))))

main = do
  msg "Running tests"
  if checks then msg   "Tests passed"
            else error "Tests failed"
  s <- fmap trim getContents
  msg ("Getting args of '" ++ s ++ "'")
  putStr (renderArgsOf s)

checks = all id [
      checkParse "nat"     (Chunk "nat"),
      checkParse "(nat)"   (Chunk "nat"),
      checkParse "a (b) c" (Group [Chunk "a", Chunk "b", Chunk "c"]),

      checkRender (Chunk "nat")                  "nat",
      checkRender (stringToType "nat")           "nat",
      checkRender (stringToType "(a => b) => c") "(a => b) => c"
    ]
  where checkParse  s want = check "Parsing"   stringToType s want
        checkRender t want = check "Rendering" show         t want

        check :: (Eq b, Show a, Show b) => String -> (a -> b) -> a -> b -> Bool
        check act f x want = let got = f x
                              in got == want || error (concat [
                                   act, " '", show x, "' gave '", show got,
                                   "' when we expected '", show want, "'"
                                 ])

{-
def args(collection):
  if collection == [] or collection == "":
    return []

  if type(collection) == type(""):
    return map(lambda s: s.strip(),
               collection.split("=>")[:-1])

  if type(collection) == type([]):
    # Discard return types from the end
    result = collection
    #while result != [] and result[-1].find('=>')

check(args, "getting args of", "nat",        [])
check(args, "getting args of", "nat => nat", ["nat"])
check(args, "getting args of", split("(nat => bool) => int"),
                               [["nat => bool"], "nat"])
-}
