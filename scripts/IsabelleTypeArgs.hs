#!/usr/bin/env runhaskell

import           Control.Applicative           ((<|>))
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy.Char8    as BS
import           Data.Char
import           Data.List                     (intercalate, nub, sort)
import           Data.Maybe                    (catMaybes)
import qualified Data.Set                      as Set
import           System.Environment            (lookupEnv)
import           System.IO
import           Test.QuickCheck
import           Text.Parsec                   (Parsec)
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.String            (Parser)
import           Text.ParserCombinators.Parsec hiding (token, (<|>))

-- Isabelle types
data Type = Chunk String | Group [Type] | Func [Type] Type
  deriving (Eq, Ord, Show)

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

testRawRender = ("can render simple types", renderType (Chunk "nat") === "nat")

testRenderParseInverse = ("can render parsed types",
                          renderType (fromRight (stringToType "nat")) === "nat")

testGroupingRegression1 = ("Grouping regression1", go)
  where go = renderType (fromRight (stringToType "(a => b) => c")) ===
             "(a => b) => c"

---

fromLeft (Left x) = x
fromLeft _        = error "Not a Left"

fromRight (Right x) = x
fromRight _         = error "Not a Right"

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

genName = listOf1 (suchThat arbitrary isAlphaNum)

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

-- Tokenise: lets us ignore whitespace, punctuation, etc.

data Token = Open | Close | Arrow | Name String deriving (Eq, Show)

instance Arbitrary Token where
  arbitrary = oneof [return Open, return Close, return Arrow,
                     Name <$> arbitrary]

token :: Parser Token
token = open <|> close <|> arrow <|> name
  where open  = char '(' >> return Open
        close = char ')' >> return Close
        arrow = string "=>" >> return Arrow
        name  = Name <$> many1 nonSpecial

parseTokens :: Parser [Token]
parseTokens = token `sepBy` spaces

nonSpecial :: Parser Char
nonSpecial = noneOf "()=> "

tokenise :: String -> Either String [Token]
tokenise s = case parse parseTokens "(unknown)" s of
  Left  err -> Left (show err)
  Right ts  -> Right ts

-- Gather up parenthesised groups

data Tree a = Leaf a | Node [Tree a] deriving (Eq, Show)

genTree 0 = Leaf <$> arbitrary
genTree n = do l <- splitList (n-1)
               Node <$> mapM genTree l

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = choose (0, 100) >>= genTree
  shrink (Leaf x) = Leaf <$> shrink x
  shrink (Node l) = Node <$> shrink l

popParens :: [Token] -> Either String (Tree Token)
popParens ts = case checkTree tree of
                 [] -> Right tree
                 es -> Left (unlines es)
  where tree = unwrap . Node . fst . go [] $ ts
        go acc []         = (reverse acc, [])
        go acc (Close:ys) = (reverse acc, ys)
        go acc (Open :ys) = let (x, rem) = go [] ys
                             in go (Node x:acc) rem
        go acc (x    :ys) =     go (Leaf x:acc) ys

testNoRemainingParens = ("no parens remaining after popping", go)
  where go args = let tokens = genBalancedTokens args
                      tree   = fromRight (popParens tokens)
                   in counterexample (show (("tokens", tokens),
                                            ("tree"  , tree)))
                                     (noParens tree)
        noParens t = case t of
          Leaf Open  -> False
          Leaf Close -> False
          Leaf _     -> True
          Node ts    -> all noParens ts

unwrap (Leaf x)        = Leaf x
unwrap (Node [Node x]) = unwrap (Node x)
unwrap (Node [Leaf x]) = Leaf x
unwrap (Node xs)       = Node (map unwrap xs)

unwrapTokens ts = if go ts == ts then ts else unwrapTokens (go ts)
  where go ts = outerUnwrap $ case ts of
          -- Base case
          [] -> []

          -- Unwrap singletons
          (Open:Name n:Close:ts') -> Name n:ts'

          -- Unwrap doubled-up parentheses
          (Open:Open:ts') -> let (pre, post) = splitWholeGroups 0 [] ts'
                              in case post of
                                   (Close:Close:xs) -> (Open:pre) ++ (Close:xs)
                                   _                -> Open : go (Open:ts')

          -- Recurse over anything else
          (t:ts') -> t : go ts'

        -- Pop tokens off ts until we hit an unmatched Close
        splitWholeGroups 0 acc (Close:ts) = (reverse acc, Close:ts)
        splitWholeGroups n acc (Close:ts) = splitWholeGroups (n-1)
                                                             (Close:acc) ts
        splitWholeGroups n acc (Open :ts) = splitWholeGroups (n+1)
                                                             (Open :acc) ts
        splitWholeGroups n acc (t    :ts) = splitWholeGroups n   (t:acc) ts
        splitWholeGroups 0 acc []         = error ("Expected Close after " ++
                                                   show (reverse acc))

        balanced n []         = n == 0
        balanced n (Close:ts) = if n < 1 then False else balanced (n-1) ts
        balanced n (Open:ts)  = balanced (n+1) ts
        balanced n (_:ts)     = balanced n ts

        outerUnwrap [] = []
        outerUnwrap ts = if balanced 0 ts
                            then let mid = tail (init ts)
                                  in if head ts == Open && last ts == Close &&
                                        balanced 0 mid
                                        then mid
                                        else ts
                            else ts

-- Returns a list of errors with the tree; an empty list means no errors
checkTree t = case t of
  Leaf Open           -> ["Unexpected ("]
  Leaf Close          -> ["Unexpected )"]
  Leaf _              -> []
  Node []             -> ["Empty group"]
  Node [Node _]       -> ["Redundant nesting"]
  Node (Leaf Arrow:_) -> ["Found prefix '=>'"]
  Node ts             | last ts == Leaf Arrow -> ["Found postfix '=>'"]
  Node ts             -> concatMap checkTree ts

testParenthesiseSimple = ("handle unparenthesised types", go)
  where go n ns = let list = stripDodgyTokens (map maybeToToken (Just n:ns))
                   in fromRight (popParens list) ===
                      unwrap (Node (map Leaf list))

testParenthesiseSingle = ("handle parenthesised types", go)
  where go pre post mid =
          let pre'   = tokens pre
              post'  = tokens post
              mid'   = tokens mid
              tokens = stripDodgyTokens . map maybeToToken
              lvs    = map Leaf
              list   = pre' ++ [Open] ++ mid' ++ [Close] ++ post'
              simpl  = Node (lvs pre' ++ [Node (lvs mid')] ++ lvs post')
           in stripDodgyTokens list == list ==> fromRight (popParens list) ===
                                                unwrap simpl

testParenthesiseNested = ("handle parentheses nesting", go)
  where go args = let tokens      = genBalancedTokens args
                      (errs, rem) = check tokens (fromRight (popParens tokens))
                   in saneTokens tokens == [] ==>
                        counterexample (show tokens)
                                       ((errs === []) .&&. (rem === []))

        check :: [Token] -> (Tree Token) -> ([String], [Token])
        check toks tree = case (toks, tree) of
          -- Base case: no errors, no remaining tokens, empty tree
          ([], Node []) -> ([], [])

          -- If a name is a leaf in the tree, pop it and recurse
          (Name n:ts, Node (Leaf (Name n'):subs)) | n == n' -> check ts
                                                                     (Node subs)

          (Name n:[], Leaf x) | Name n == x -> ([], [])

          -- If '=>' is a leaf in the tree, pop it and recurse
          (Arrow:ts, Node (Leaf Arrow:subs)) -> check ts (Node subs)

          -- If there's a '(', there should be a sub-tree; check it then recurse
          (Open:ts,   Node (Node s:ss)) -> let (e1, ts2) = check ts  (Node s)
                                               (e2, ts3) = check ts2 (Node ss)
                                            in (e1 ++ e2, ts3)

          -- Singular names might get unwrapped
          (Open:Name n:Close:ts, Node (Leaf (Name m):t)) | n == m -> check
                                                                       ts
                                                                       (Node t)

          -- Closing should coincide with an empty tree
          (Close:ts, Node []) -> ([], ts)

          -- Any other combination is a mismatch
          (_, Leaf _) -> (["Unexpected leaf: "      ++ show (toks, tree)], [])
          ([], _)     -> (["Stopped unexpectedly: " ++ show (toks, tree)], [])
          (_, _)      -> (["Mismatch: "             ++ show (toks, tree)], [])

---

genBalancedTokens (ns, n, is, i) = unwrapTokens . stripDodgyTokens . pad $
                                     mkTs 0 (map maybeToToken (Just n:ns))
                                            (sort os)
                                            (sort cs)
  where -- Turn ints 'is' into (an even number of) indices into 'ns'
        ints    = is ++ if odd (length is)
                           then [i]  -- Pad odd length with extra int
                           else [ ]  -- Leave even length alone

        indices = map ((`mod` (length ns + 1)) . abs) ints

        -- Use half of 'indices' as positions of open parens and half as close.
        -- Since 'indices' has even length, the parentheses will balance.
        (os, cs) = splitAt (length indices `div` 2) indices

        -- Recurse through list of tokens 't'. If index 'n' is in list of open
        -- positions 'o', put an 'Open' token; if it's in list of close
        -- positions 'c', put a 'Close' token.
        mkTs n t o c = case (n `elem` o, n `elem` c, t) of
          (True, _   , _  ) -> Open  : mkTs  n    t (tail o)       c
          (_   , True, _  ) -> Close : mkTs  n    t       o  (tail c)
          (_   , _   , u:v) -> u     : mkTs (n+1) v       o        c
          (_   , _   , [] ) -> []

        -- Insert 'dummy' in between '=>)', '(=>', '=>=>' and '()'
        dummy  = Name n
        pad ts = case ts of
          Open  : Close : us -> Open  : dummy : pad (Close : us)
          Open  : Arrow : us -> Open  : dummy : pad (Arrow : us)
          Arrow : Close : us -> Arrow : dummy : pad (Close : us)
          Arrow : Arrow : us -> Arrow : dummy : pad (Arrow : us)
          t             : us -> t             : pad          us
          []                 -> []

testBalancedTokensSane = ("generator makes sane, balanced tokens", go)
  where go args = let tokens = genBalancedTokens args
                      errs   = saneTokens tokens
                   in counterexample (show tokens) (errs === [])

saneTokens ts = saneSeqs   ts        ++
                (let c = nestCount ts 0
                  in if c == 0 then [] else ["Mismatched parens " ++ show c]) ++
                startsWell ts        ++
                endsWell   ts
  where saneSeqs ts = case ts of
          []                             -> []
          [Arrow]                        -> ["Suffixed with arrow"]
          [Open]                         -> ["Suffixed with Open"]
          (Open   : Close :          ts) -> ["Empty parens ()"]
          (Open   : Arrow :          ts) -> ["Found (=>"]
          (Arrow  : Close :          ts) -> ["Found =>)"]
          (Arrow  : Arrow :          ts) -> ["Found => =>"]
          (Name _ : Arrow : Name n : ts) -> saneSeqs (Name n:ts)
          (Name _ : Arrow : Open   : ts) -> saneSeqs (Open  :ts)
          (Close  : Arrow : Name n : ts) -> saneSeqs (Name n:ts)
          (Close  : Arrow : Open   : ts) -> saneSeqs (Open  :ts)
          (_      :                  ts) -> saneSeqs ts

        nestCount []         n = n
        nestCount (Open :ts) n = nestCount ts (n+1)
        nestCount (Close:ts) n = if n > 0 then nestCount ts (n-1) else -1
        nestCount (_    :ts) n = nestCount ts n

        balanced ts = let opens  = length (filter (== Open)  ts)
                          closes = length (filter (== Close) ts)
                       in if opens == closes
                             then []
                             else ["Mismatched parens: " ++
                                   show (("Open", opens), ("Close", closes))]

        startsWell ts = case ts of
          Close:_ -> ["Closes before opening"]
          Arrow:_ -> ["Prefixed with arrow"]
          _       -> []

        endsWell ts = case reverse ts of
          Open :_ -> ["Opens without closing"]
          Arrow:_ -> ["Suffixed with arrow"]
          _       -> []

maybeToToken Nothing  = Arrow
maybeToToken (Just n) = Name n

stripDodgyTokens ts = let stripped = go (pad 0 (dropWhile (== Arrow) ts))
                       in if stripped == ts
                             then ts
                             else stripDodgyTokens stripped
  where go (Arrow : Arrow : ts) = go (Arrow:ts)
        go (Open  : Arrow : ts) = go (Open:ts)
        go (Open  : Close : ts) = go ts
        go (Arrow : Close : ts) = go (Close:ts)
        go [Arrow]              = []
        go [Open]               = error "Suffixed Open"
        go (t:ts)               = t : go ts
        go []                   = []

        pad n ts = case ts of
          []       -> replicate n Close    -- Close off any remaining Opens
          Open :ts -> Open : pad (n+1) ts  -- Increment depth counter
          Close:ts -> if n < 1
                         then         pad  n    ts -- Not enough Opens, drop it
                         else Close : pad (n-1) ts -- Keep and recurse
          t    :ts -> t : pad n ts

testStripMakesSane = ("strip to sane tokens", go)
  where go ts = let tokens = stripDodgyTokens ts
                 in counterexample (show tokens) (saneTokens tokens === [])

-- Gather up function arguments

gatherArgs :: [Tree Token] -> [[Tree Token]]
gatherArgs l = let (pre, post) = break (== Leaf Arrow) l
                in pre : if null post then [] else gatherArgs (tail post)

testGatherArgs = ("can pick out function args", forAll genList go)
  where go ts = let ts'   = filter (Leaf Arrow `notElem`) ts
                    input = intercalate [Leaf Arrow] ts'
                 in gatherArgs input === if null ts' then [[]] else ts'

        genList = do n  <- choose (0, 100)
                     l  <- splitList n
                     l' <- mapM splitList l
                     mapM (mapM genTree) l'

testGatherArgsNoArrows = ("gathering args eliminates arrows", go)
  where go ts = let ts' = gatherArgs ts
                 in counterexample (show ts') (all (Leaf Arrow `notElem`) ts')

tokensToType :: [Token] -> Either String Type
tokensToType ts = do tree <- popParens ts
                     treeToType tree

testTokensToValidType = ("tokens parse to type", go)
  where go args = let tokens      = genBalancedTokens args
                      Right tree  = popParens tokens
                      Right type' = tokensToType tokens
                      args2       = case tree of
                        Leaf _  -> Nothing
                        Node ts -> Just (gatherArgs ts)
                   in validTree tree ==>
                      counterexample (show (("tokens", tokens),
                                            ("tree"  , popParens tokens),
                                            ("args"  , args2),
                                            ("type"  , type')))
                                     (allNamesFound type' tokens .&&.
                                      notDodgy      type')

        allNamesFound ty = all (\t -> case t of
                                        Name n -> inType n ty
                                        _      -> True)

        inType n t = case t of
          Chunk m   -> n == m
          Group ts  -> any (inType n) ts
          Func ts t -> any (inType n) (t:ts)

        notDodgy t = case t of
          Chunk n   -> n `notElem` ["Bare =>", "Bare (", "Bare )"]
          Group ts  -> all notDodgy ts
          Func ts t -> all notDodgy (t:ts)

        validTree (Leaf (Name _)) = True
        validTree (Node _)        = True
        validTree _               = False

---

treeToType :: Tree Token -> Either String Type
treeToType t = case t of
    Leaf Arrow    -> Left "Bare =>"
    Leaf Open     -> Left "Bare ("
    Leaf Close    -> Left "Bare )"
    Leaf (Name n) -> Right (Chunk n)
    Node ts       -> case gatherArgs ts of
      []  -> Left "Empty node"
      [g] -> Group <$> mapM treeToType g
      ts' -> do ts''  <- mapM group (init ts')
                ts''' <- mapM treeToType ts''
                lst   <- group (last ts')
                lst'  <- treeToType (lst)
                return (Func ts''' lst')
  where group []  = Left "Can't group []"
        group [x] = Right x
        group xs  = Right (Node xs)

testTreeToType = ("can turn trees into types", go)
  where go tree = let tree'       = sanitiseTree (stripParens tree)
                      Just sane   = tree'
                      Right type' = treeToType sane
                   in (tree' /= Nothing) ==> all (nameIn type') (namesIn sane)

        stripParens t = case t of
          Leaf Open  -> Leaf Arrow
          Leaf Close -> Leaf Arrow
          Leaf _     -> t
          Node ls    -> Node (map stripParens
                                  (filter (`notElem` [Leaf Open, Leaf Close])
                                          ls))

        nameIn ty n = case ty of
          Chunk m   -> n == m
          Group g   -> any (`nameIn` n) g
          Func  i o -> any (`nameIn` n) (o:i)

        namesIn t = case t of
          Leaf (Name n) -> [n]
          Leaf _        -> []
          Node ts       -> concatMap namesIn ts

sanitiseTree t = case t of
    Leaf Arrow -> Just dummy
    Leaf _     -> Just t
    Node []    -> Just (Node [dummy])
    Node ts    -> Node <$> sanitiseList ts
  where dummy           = Leaf (Name "sane")
        sanitiseList ts = case ts of
          -- Base case for recursion, after popping off valid trees
          [] -> Just []

          -- Arrows aren't allowed at the start or end of a list
          (Leaf Arrow:x)            -> (dummy:) <$> sanitiseList x
          _ | last ts == Leaf Arrow -> (++ [dummy]) <$> sanitiseList (init ts)

          -- Arrows are only allowed between things
          (Leaf (Name x):Leaf Arrow:Leaf (Name y):z) ->
            do l <- sanitiseList (Leaf (Name y):z)
               return (Leaf (Name x):Leaf Arrow:l)

          (Leaf (Name x):Leaf Arrow:Node y:z)        ->
            do y' <- sanitiseTree (Node y)
               l  <- sanitiseList (y':z)
               return (Leaf (Name x):Leaf Arrow:l)

          (Node x:Leaf Arrow:Leaf (Name y):z)        ->
            do x' <- sanitiseTree (Node x)
               l  <- sanitiseList (Leaf (Name y):z)
               return (x':Leaf Arrow:l)

          (Node x:Leaf Arrow:Node y:z)               ->
            do x' <- sanitiseTree (Node x)
               y' <- sanitiseTree (Node y)
               l  <- sanitiseList (y':z)
               return (x':Leaf Arrow:l)

          -- Recurse into Nodes
          (Node x:y) -> do x' <- sanitiseTree (Node x)
                           l  <- sanitiseList y
                           return (x':l)

          -- Allow names
          Leaf (Name x):y -> (Leaf (Name x):) <$> sanitiseList y

          -- Disallow anything else
          _ -> Nothing

---

stringToType :: String -> Either String Type
stringToType s = do tokens <- tokenise s
                    type'  <- tokensToType tokens
                    return (simplify type')

testParseSimple = ("can parse simple types",
                   stringToType "nat" === Right (Chunk "nat"))

testParseParens = ("can parse parenthesised",
                   stringToType "(nat)" === Right (Chunk "nat"))

testParseSpaced = ("can parse spaced",
                   stringToType "a (b) c" ===
                     Right (Group [Chunk "a", Chunk "b", Chunk "c"]))

testParseRegression1 = ("no parse regression",
                        stringToType "A B => (A B) B => C B" ===
                          Right (Func [Group [Chunk "A", Chunk "B"],
                                       Group [Group [Chunk "A", Chunk "B"],
                                              Chunk "B"]]
                                      (Group [Chunk "C", Chunk "B"])))

testParseRenderInverse = ("parse is the inverse of renderType", go)
  where go t = stringToType (renderType t) === Right (simplify t)

testSpacesGroup = ("spaces group", forAll ((,) <$> genName <*> genName) go)
  where go (x, y) = stringToType (x ++ " " ++ y) === Right (Group [Chunk x,
                                                                   Chunk y])

---

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

-- Pull types (nullary or fully-applied) out of functions, since functions can't
-- be given as datatypes to IsaCoSy.
namedOf :: Type -> [Type]
namedOf t = case t of
  -- Keep standalone chunks
  Chunk _       -> [t]

  -- Recurse through function arg/return types
  Func args ret -> namedOf ret ++ concatMap namedOf args

  -- Assume that a group is fully-applied. Recurse into each parameter, since we
  -- also assume they're fully-applied. Note that Isabelle types put the
  -- constructor last, like in ML.
  Group ts      -> t : case ts of
                         [] -> []
                         _  -> concatMap namedOf (init ts)

testNamedOfNullary = ("one name from nullary",
                      Chunk "nat" `hasNamed` [Chunk "nat"])

testNamedOfFunction = ("get named of function",
                       Func [Chunk "i"] (Chunk "o")
                       `hasNamed` [Chunk "i", Chunk "o"])

testNamedOfNested = ("get named from nested functions",
                     Func [Func [Chunk "nat"] (Chunk "bool")] (Chunk "int")
                     `hasNamed` [Chunk "nat", Chunk "bool", Chunk "int"])

testNamedOfParams = ("get named from parameterised types",
                     Func [Group [Chunk "nat", Chunk "list"]] (Chunk "int")
                     `hasNamed` [Group [Chunk "nat", Chunk "list"],
                                 Chunk "int", Chunk "nat"])

---

hasNamed t ts = namedOf t `setEq` ts

setEq xs ys = Set.fromList xs === Set.fromList ys

renderNamedOf :: String -> [String]
renderNamedOf s = map renderType (nub (namedOf (case stringToType s of
                                                   Left  e -> error e
                                                   Right t -> t)))

main = do mode <- lookupEnv "RUN_TESTS"
          case mode of
            Nothing -> runParser
            Just _  -> runTests
  where runTests = do msg "Running tests"
                      ic <- check
                      case ic of
                        []   -> msg   "Tests passed"
                        errs -> mapM_ msg errs >> error "Tests failed"

        runParser = do raw <- BS.getContents
                       let ts = case Aeson.eitherDecode raw of
                                  Left err -> error (show (
                                                "message", "Failed parsing",
                                                "error",   err,
                                                "stdin",   raw))
                                  Right ts -> ts
                       msg ("Getting named types from " ++ show (length ts)
                                                        ++ " types")
                       let named = concatMap (renderNamedOf . trim)
                                             (ts :: [String])
                       BS.putStr (Aeson.encode named)

msg = hPutStrLn stderr

check = concat <$> sequence [go testStripMakesSane,
                             go testBalancedTokensSane,
                             go testRawRender,
                             go testRenderParseInverse,
                             go testGroupingRegression1,
                             go testParenthesiseSimple,
                             go testParenthesiseSingle,
                             go testParenthesiseNested,
                             go testParseSimple,
                             go testParseParens,
                             go testParseSpaced,
                             go testParseRegression1,
                             go testParseRenderInverse,
                             go testSpacesGroup,
                             go testNamedOfNullary,
                             go testNamedOfFunction,
                             go testNamedOfNested,
                             go testNamedOfParams,
                             go testGatherArgs,
                             go testTokensToValidType,
                             go testNoRemainingParens,
                             go testGatherArgsNoArrows,
                             go testTreeToType]
  where go (m, p) = do msg ("Checking " ++ m)
                       handle m <$> quickCheckResult p
        handle m got = case got of
                         Success{} -> []
                         GaveUp{}  -> ["Gave up checking " ++ m]
                         Failure{} -> [ "Failed checking " ++ m]
                         _         -> [ "Failed checking " ++ m]
