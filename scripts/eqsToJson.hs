#!/usr/bin/env runhaskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Applicative           ((*>), (<*), (<|>))
import           Data.Aeson
import           Data.Aeson.Types              hiding (Parser, parse)
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Char
import           Data.List                     (elemIndex, intercalate, nub)
import           Data.Maybe                    (fromJust)
import           Numeric
import           Prelude                       hiding (Eq)
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

data Eq   a = Eq (Expr a) (Expr a)

data Expr a = Const String | Var (Var a) | App (Expr a) (Expr a) | Lam (Expr a)

data Var  a = Free a | Bound Int

varIndex (Free  i) = toJSON i
varIndex (Bound i) = toJSON i

isBound (Bound _) = True
isBound (Free  _) = False

instance ToJSON (Eq Int) where
  toJSON (Eq lhs rhs) = object ["relation" .= String "~=",
                                "lhs"      .= toJSON lhs ,
                                "rhs"      .= toJSON rhs ]

instance ToJSON a => ToJSON (Expr a) where
  toJSON e = object (case e of
                       Const s -> ["role"   .= String "constant"   ,
                                   "type"   .= String "unknown"    ,
                                   "symbol" .= s                   ]
                       Var v   -> ["role"   .= String "variable"   ,
                                   "type"   .= String "unknown"    ,
                                   "bound"  .= Bool   (isBound v)  ,
                                   "id"     .= varIndex v          ]
                       App f x -> ["role"   .= String "application",
                                   "lhs"    .= toJSON f            ,
                                   "rhs"    .= toJSON x            ])

-- Needed by 'notFollowedBy', for some reason
instance Show (Expr String) where
  show = B.unpack . encode

stringToEqs :: String -> [Eq Int]
stringToEqs = map stringToEq . filter (/= "") . lines

stringToEq :: String -> Eq Int
stringToEq s = case (parse parseExpr "lhs" lhs, parse parseExpr "rhs" rhs) of
    (Left  err, _        ) -> fail lhs err
    (_        , Left  err) -> fail rhs err
    (Right lhs, Right rhs) -> numberEq (Eq lhs rhs)
  where (lhs, rhs)   = (trim       (takeWhile (/= '=') s),
                        trim (tail (dropWhile (/= '=') s)))
        fail str err = error ("Couldn't parse '" ++ str ++ "': " ++ show err)

trim = reverse . trimLeading . reverse . trimLeading
  where trimLeading = dropWhile isSpace

parseExpr :: Parser (Expr String)
parseExpr = chainl1 parseNonApp (space >> return App) <?> "parseExpr"

parseNonApp = (parseGroup <|> try parseConst <|> parseVar) <?> "parseNonApp"

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
numberEq (Eq lhs rhs) = Eq (numberExpr db lhs) (numberExpr db rhs)
  where db = nub (collectVars lhs ++ collectVars rhs)

collectVars :: Expr String -> [String]
collectVars e = case e of
  Const _        -> []
  Var   (Free s) -> [s]
  App   f x      -> collectVars f ++ collectVars x

numberExpr :: [String] -> Expr String -> Expr Int
numberExpr db e = case e of
  Const s        -> Const s
  Var   (Free s) -> Var (Free (fromJust (elemIndex s db)))
  App   f x      -> App (numberExpr db f) (numberExpr db x)

-- Uses TemplateHaskell to look up definitions prefixed with 'prop_'
return []
runTests = $quickCheckAll

main :: IO ()
main = lookupEnv "RUN_TESTS" >>= \case
  Nothing -> B.interact (encode . stringToEqs . B.unpack)
  Just _  -> runTests >>= \case
    True  -> putStrLn "All tests pass"
    False -> error    "Test suite failed"
