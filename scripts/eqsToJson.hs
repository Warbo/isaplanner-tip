#!/usr/bin/env runhaskell
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

import           Control.Applicative           ((*>), (<*), (<|>))
import           Data.Aeson
import           Data.Aeson.Types              hiding (Parser, parse)
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Char
import           Data.List                     (elemIndex, intercalate, nub)
import           Data.Maybe                    (fromJust)
import           Numeric
import           Prelude                       hiding (Eq)
import qualified Prelude
import           System.Environment            (lookupEnv)
import           System.IO
import           Test.QuickCheck
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Expr
import           Text.Parsec.String            (Parser)
import           Text.ParserCombinators.Parsec hiding ((<|>))

-- The parameter is how we index variables: we read them as strings, but output
-- them as integers.

data Eq   a = Eq (Expr a) (Expr a) deriving (Prelude.Eq)

-- 'Var' is either bound (by a Lam) or free. A 'Lam' with 'Just foo' binds the
-- variable named 'foo', whilst a 'Lam' with 'Nothing' uses de Bruijn indices.
data Expr a = Const String | Var (Var a) | App (Expr a) (Expr a) |
              Lam (Maybe a) (Expr a)
     deriving (Prelude.Eq)

data Var  a = Free a | Bound Int
     deriving (Prelude.Eq, Show)

instance (ToJSON a) => ToJSON (Eq a) where
  toJSON (Eq lhs rhs) = object ["relation" .= String "~=",
                                "lhs"      .= toJSON lhs ,
                                "rhs"      .= toJSON rhs ]

instance ToJSON a => ToJSON (Expr a) where
  toJSON e = object (case e of
                       Const s -> ["role"   .= String "constant"    ,
                                   "type"   .= String "unknown"     ,
                                   "symbol" .= s                    ]
                       Var v   -> ["role"   .= String "variable"    ,
                                   "type"   .= String "unknown"     ,
                                   "bound"  .= Bool   (isBound v)   ,
                                   "id"     .= varIndex v           ]
                       App f x -> ["role"   .= String "application" ,
                                   "lhs"    .= toJSON f             ,
                                   "rhs"    .= toJSON x             ]
                       Lam v x -> ["role"   .= String "lambda"      ,
                                   "arg"    .= maybe Null toJSON v  ,
                                   "body"   .= toJSON x             ])

instance (ToJSON a) => Show (Eq a) where
  show = B.unpack . encode

-- Needed by 'notFollowedBy', for some reason
instance (ToJSON a) => Show (Expr a) where
  show = B.unpack . encode

stringToEqs :: String -> [Eq Int]
stringToEqs = map stringToEq . filter (/= "") . lines

prop_stringToEqs =
  stringToEqs (unlines [
    "globalAA x = globalBB",
    "",
    "globalCC (globalDD ?x ?y) = globalEE ?y ?y"]) ===
  [Eq (App (Const "globalAA") (Var (Free 0))) (Const "globalBB"),
   Eq (App (Const "globalCC")
           (App (App (Const "globalDD") (Var (Free 0)))
                (Var (Free 1))))
      (App (App (Const "globalEE") (Var (Free 1))) (Var (Free 1)))]

stringToEq :: String -> Eq Int
stringToEq s = case (parse parseExpr "lhs" lhs, parse parseExpr "rhs" rhs) of
    (Left  err, _        ) -> fail lhs err
    (_        , Left  err) -> fail rhs err
    (Right lhs, Right rhs) -> numberEq (Eq lhs rhs)
  where (lhs, rhs)   = (trim       (takeWhile (/= '=') s),
                        trim (tail (dropWhile (/= '=') s)))
        fail str err = error ("Couldn't parse '" ++ str ++ "': " ++ show err)

prop_stringToEq_lambdas =
  stringToEq "global64 (global65 (%a. ?a) Global6c) ?b = ?b" ===
  (Eq (App (App (Const "global64") (App (App (Const "global65")
                                             (Lam Nothing (Var (Bound 0))))
                                        (Const "Global6c")))
           (Var (Free 0)))
      (Var (Free 0)))

prop_stringToEq_curries =
  stringToEq "globalaa (%x y. ?x) = globalbb" ===
  (Eq (App (Const "globalaa") (Lam Nothing (Lam Nothing (Var (Bound 1)))))
      (Const "globalbb"))

trim = reverse . trimLeading . reverse . trimLeading
  where trimLeading = dropWhile isSpace

parseExpr :: Parser (Expr String)
parseExpr = chainl1 parseNonApp (space >> return App) <?> "parseExpr"

parseNonApp = (try parseLam <|> parseGroup <|> try parseConst <|> parseVar)
              <?> "parseNonApp"

parseLam = between (char '(') (char ')') go <|> go <?> "parseLam"
  where go = do char '%'
                vars <- sepBy1 (many1 (noneOf ". ")) space
                char '.'
                spaces
                body <- parseExpr
                return (nestLams vars body)
        nestLams []     body = body
        nestLams (v:vs) body = Lam (Just v) (nestLams vs body)

parseGroup :: Parser (Expr String)
parseGroup = between (char '(') (char ')') parseExpr <?> "parseGroup"

parseConst :: Parser (Expr String)
parseConst = do
      pre <- string "global" <|> string "Global"
      hex <- many1 hexPair
      return (Const (pre ++ concat hex))
    <?> "parseConst"
  where hexPair = do { x <- hexDigit; y <- hexDigit; return [x, y]; }

parseVar :: Parser (Expr String)
parseVar = do
    optional (char '?')
    id <- many1 letter
    return (Var (Free id))
  <?> "parseVar"

numberEq :: Eq String -> Eq Int
numberEq (Eq lhs rhs) = Eq (numberExpr db [] lhs) (numberExpr db [] rhs)
  where db = nub (collectVars [] lhs ++ collectVars [] rhs)

collectVars :: [String] -> Expr String -> [String]
collectVars bound e = case e of
  Const _        -> []
  Var (Free s)   -> if s `elem` bound then [] else [s]
  App f x        -> collectVars bound f ++ collectVars bound x
  Lam (Just v) x -> collectVars (v:bound) x

numberExpr :: [String] -> [String] -> Expr String -> Expr Int
numberExpr db env e = case e of
  -- Constants don't contain variables
  Const s -> Const s

  -- Recurse into applications, without changing the context
  App f x -> App (numberExpr db env f) (numberExpr db env x)

  -- All variables come in as 'Free'; look them up in 'env' (bound locals) first
  -- and fall back to 'db' (free globals) if not found
  Var (Free s) -> case (elemIndex s env, elemIndex s db) of
                    (Just i , _     ) -> Var (Bound i)
                    (Nothing, Just i) -> Var (Free  i)
                    _                 -> error ("Unknown var " ++ show s)

  -- If there are bound variables in our input, something is wrong
  Var (Bound s) -> error ("Prematurely bound variable " ++ show s)

  -- Recurse into lambdas, but extend the environment. The result will use
  -- de Bruijn indices.
  Lam (Just s) x -> Lam Nothing (numberExpr db (s:env) x)

  -- If there are de Bruijn lambdas in our input, something is wrong
  Lam Nothing x -> error ("Prematurely de-Bruijn-transformed lambda " ++ show x)

varIndex (Free  i) = toJSON i
varIndex (Bound i) = toJSON i

isBound (Bound _) = True
isBound (Free  _) = False

-- Uses TemplateHaskell to look up definitions prefixed with 'prop_'
return []
runTests = $quickCheckAll

main :: IO ()
main = lookupEnv "RUN_TESTS" >>= \case
  Nothing -> B.interact (encode . stringToEqs . B.unpack)
  Just _  -> runTests >>= \case
    True  -> putStrLn "All tests pass"
    False -> error    "Test suite failed"
