#!/usr/bin/env runhaskell

import           Control.Applicative
import           Data.Char
import           Data.List                     (intercalate, nub)
import           Data.Maybe                    (catMaybes)
import           System.Environment            (lookupEnv)
import           System.IO
import           Test.QuickCheck
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.String            (Parser)
import           Text.ParserCombinators.Parsec hiding ((<|>))

msg = hPutStrLn stderr

-- Isabelle types
data Type = Chunk String | Group [Type] | Func [Type] Type deriving (Eq, Show)

renderType t = case t of
      Chunk s        -> s
      Group xs       -> unwords (map wrap xs)
      Func  args ret -> intercalate " => " (map wrap (args ++ [ret]))
    where needParens (Group _)  = True
          needParens (Func _ _) = True
          needParens _          = False
          wrap x = if needParens x
                      then "(" ++ renderType x ++ ")"
                      else        renderType x

splitList :: Int -> Gen [Int]
splitList n | n <= 0 = return []
splitList n          = do m <- choose (1, n)
                          (m:) <$> splitList (n - m)

genType :: Int -> Gen Type
genType n = case n of
    0 -> Chunk <$> genName
    _ -> oneof [Chunk <$> genName,
                do ints  <- splitList (n - 1)
                   types <- mapM genType ints
                   case types of
                     [] -> genType (n - 1)  -- Don't generate empty groups
                     _  -> return (Group types),
                do ints <- splitList (n - 1)
                   args <- mapM genType ints
                   case args of
                     r:a:as -> return (Func (a:as) r)
                     _      -> genType (n - 1)]  -- Don't have empty args
  where genName = listOf1 (suchThat arbitrary isAlphaNum)

instance Arbitrary Type where
  arbitrary = choose (0, 100) >>= genType
  shrink t  = case t of
      -- Simplify a Chunk's name. Try "x", unless we already have, to avoid
      -- punctuation red-herrings. Otherwise shrink the name and avoid empties.
      Chunk s  -> if s == "x"
                     then []
                     else Chunk "x" : if length s > 1
                                         then let (x, y) = splitList s
                                               in [Chunk x, Chunk y]
                                         else []

      -- We need at least one Group element. If we have only one, try shrinking
      -- it. Otherwise, try using just the first or second half of the list.
      Group ts -> case ts of
                    []  -> error "Shouldn't make empty group"
                    [t] -> t : map (Group . return) (shrink t)
                    _   -> let (pre, post) = splitList ts
                            in [Group pre, Group post]

      -- Similar to Group: shrink solitary arguments, otherwise try each half of
      -- the list.
      Func ts r -> case ts of
                     []  -> error "Shouldn't make empty Func args"
                     [t] -> t : map (\(t, r) -> Func [t] r) (shrink (t, r))
                     _   -> let (pre, post) = splitList ts
                             in [Func pre r, Func post r]

    where splitList l = let mid = length l `div` 2
                         in (take mid l, drop mid l)

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

stringToType :: String -> Either String Type
stringToType s = case parse parseType "(unknown)" ("(" ++ s ++ ")") of
  Left  err -> Left  ("Couldn't parse '" ++ s ++ "': " ++ show err)
  Right t   -> Right (simplify t)

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
renderArgsOf s = unlines (map renderType (nub (argsOf (case stringToType s of
                                                         Left  e -> error e
                                                         Right t -> t))))

main = do mode <- lookupEnv "RUN_TESTS"
          case mode of
            Nothing -> runParser
            Just _  -> runTests
  where runTests = do msg "Running tests"
                      pass <- checks
                      if pass then msg   "Tests passed"
                              else error "Tests failed"
        runParser = do s <- fmap trim getContents
                       msg ("Getting args of '" ++ s ++ "'")
                       putStr (renderArgsOf s)

checks = do
    ic <- impureChecks
    case pureChecks ++ ic of
      []   -> return True
      errs -> mapM_ msg errs >> return False

impureChecks = do
    runQuickCheck "parse is the inverse of renderType" parseShowInverse
  where parseShowInverse t = stringToType (renderType t) === Right (simplify t)
        runQuickCheck m p = do
          got <- quickCheckWithResult (stdArgs { chatty = False }) p
          return $ case got of
            Success{} -> []
            GaveUp{}  -> ["Gave up checking " ++ m]
            Failure{} -> ["Failed checking "  ++ m, output got]
            _         -> ["Failed checking "  ++ m, show got]

pureChecks = catMaybes [
      checkParse "nat"     (Chunk "nat"),
      checkParse "(nat)"   (Chunk "nat"),
      checkParse "a (b) c" (Group [Chunk "a", Chunk "b", Chunk "c"]),

      -- Regression test
      checkParse "A B => (A B) B => C B"
                 (Func [Chunk "A B", Group [Chunk "A B", Chunk "B"]]
                       (Chunk "C B")),

      checkRender (Right (Chunk "nat"))          "nat",
      checkRender (stringToType "nat")           "nat",
      checkRender (stringToType "(a => b) => c") "(a => b) => c",

      let a = argsOf (Chunk "nat")
       in if a == []
          then Nothing
          else Just ("'nat' args are " ++ show a),

      let a = argsOf (Func [Chunk "nat"] (Chunk "nat"))
       in if a == [Chunk "nat"]
          then Nothing
          else Just ("'nat => nat' args are " ++ show a),

      let a = argsOf (Func [Func [Chunk "nat"] (Chunk "bool")] (Chunk "int"))
       in if a == [Func [Chunk "nat"] (Chunk "bool"), Chunk "nat"]
          then Nothing
          else Just ("'(nat => bool) => int' args are " ++ show a)
    ]
  where checkParse  s          x = check "Parsing"   stringToType s (Right x)
        checkRender (Right t)  x = check "Rendering" renderType   t x
        checkRender (Left err) x = Just err

        -- 'Nothing' on success, 'Just errorMessage' on failure
        check :: (Eq b, Show a, Show b) => String
                                        -> (a -> b)
                                        -> a
                                        -> b
                                        -> Maybe String
        check act f x want =
          let got = f x
           in if got == want
                 then Nothing
                 else Just (concat [ act, " '", show x, "' gave '", show got,
                                     "' when we expected '", show want, "'" ])
