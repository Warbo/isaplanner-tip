#!/usr/bin/env runhaskell
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}

import           Control.Applicative           ((<|>))
import qualified Data.ByteString.Lazy.Char8    as BS
import           Numeric.Natural
import           System.Environment            (lookupEnv)
import           System.IO                     (hPutStrLn, stderr)
import           Test.QuickCheck
import           Text.Parsec                   (Parsec)
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.String            (Parser)
import           Text.ParserCombinators.Parsec hiding (token, (<|>))

main = lookupEnv "RUN_TESTS" >>= \case
    Nothing -> runParser
    Just _  -> runTests
  where runParser = BS.interact parseAndRender
        runTests  = check >>= \case
          []   -> msg "Tests passed"
          errs -> mapM msg errs >> error "Tests failed"

msg = hPutStrLn stderr

check :: IO [String]
check = concat <$> sequence [
      run testCompileMessage,
      run testWarning
    ]
  where run (m, t) = do msg ("Checking " ++ m)
                        quickCheckResult t >>= \case
                          Success{} -> return []
                          GaveUp{}  -> return ["Gave up checking " ++ m]
                          _         -> return ["Failed checking "  ++ m]

parseAndRender :: BS.ByteString -> BS.ByteString
parseAndRender x = case parseAndRender' x of
  Left err -> error err
  Right y  -> y

parseAndRender' :: BS.ByteString -> Either String BS.ByteString
parseAndRender' x = case parse parseOutput "stdin" x of
  Left err -> Left  (show err)
  Right ts -> Right (error "NOT IMPLEMENTED")

parseOutput = fmap concat (message `sepBy` spaces)

message = (compileMessage >> return []) <|> (fmap (:[]) warning)

compileMessage = between (char '[') (char ']') (many1 (noneOf "]")) >>
                 string " Compiling " >> many1 (noneOf "\n") >> return []

testCompileMessage = ("can skip compile messages", go)
  where go = case parse compileMessage "test" m of
          Left err -> error (show err)
          Right _  -> True
        m  = "[1 of 1] Compiling A                ( A.hs, A.o )"

warning :: Parsec BS.ByteString _ Message
warning = do colonSep [filename, line, column]
             spaces
             spaceSep [string "Warning:",
                       string "Pattern match(es) are non-exhaustive",
                       string "In an equation for"]
             spaces
             fName <- funcName
             spaces
             string "Patterns not matched:"
             spaces
             pats <- sepBy1 pat spaces
             return (Msg { msgName = BS.pack fName, msgPats = pats })

  where filename = many1 (noneOf ":")
        line     = many1 digit
        column   = many1 digit

        funcName = char '`' *> many1 (noneOf "'") <* oneOf "'" <* char ':'

        pat = do string "Global"
                 hex <- many1 (oneOf "0123456789abcdefABCDEF")
                 spaces
                 args <- char '_' `sepBy` spaces
                 return (Pat { patName = BS.append "Global" (BS.pack hex),
                               patArgs = fromIntegral (length args) })

        -- Given [a, b, c] these match 'a:b:c:' and 'a b c '
        colonSep = seqSep (char ':')
        spaceSep = seqSep spaces
        seqSep s = \case
          []   -> return (error "No result from seqSep")
          p:ps -> p >> s >> seqSep s ps

testWarning = ("match warnings", go)
  where go = conjoin ts

        ts :: [Property]
        ts = map test
                 [[("Globaldeadbeef",      0)                     ],
                  [("Globaldead",          0), ("Globalbeef",   0)],
                  [("Globaldeadbeef _\n_", 2)                     ],
                  [("Globaldead _\n_\n_",  3), ("Globalbeef _", 1)]]

        test :: [(BS.ByteString, Natural)] -> Property
        test ps = case parse warning "test" (BS.unlines (pre ++ map fst ps)) of
          Left err -> error (show err)
          Right m  -> (msgName m === "global123") .&&.
                      (msgPats m === map mkPat ps)

        pre :: [BS.ByteString]
        pre = ["test.hs:1:2: Warning:",
               "Pattern match(es) are non-exhaustive",
               "In an equation for `global123':",
               "Patterns not matched:"]

        mkPat :: (BS.ByteString, Natural) -> Pattern
        mkPat (string, arity) = Pat { patName = head (BS.words string),
                                      patArgs = arity }

data Message = Msg { msgName :: BS.ByteString, msgPats :: [Pattern] }
     deriving (Eq, Show)

data Pattern = Pat { patName :: BS.ByteString, patArgs :: Natural   }
     deriving (Eq, Show)

instance Arbitrary Message where
  arbitrary = Msg <$> genName <*> listOf arbitrary
    where genName = do Hex h <- arbitrary
                       return (BS.append "global" h)

  shrink (Msg name pats) = map mkMsg (shrink (hexName, pats))
    where mkMsg (Hex h, ps) = Msg (BS.append "global" h) ps
          hexName = Hex (BS.drop 6 name)

instance Arbitrary Pattern where
  arbitrary = Pat <$> genName <*> genArgs
    where genName = do Hex h <- arbitrary
                       return (BS.append "Global" h)
          genArgs = fmap (fromInteger . abs) arbitrary

newtype Hex = Hex BS.ByteString

instance Arbitrary Hex where
  arbitrary = do hex <- listOf1 genHexit
                 pad <- genHexit
                 return (BS.pack (if even (length hex)
                                     then hex
                                     else pad : hex))
    where genHexit = elements "0123456789abcdefABCDEF"

  shrink h = if hexLen > 2
                then [hexPad hexPre, hexPad hexPost]
                else []
    where hexLen   = BS.length hex `div` 2
          hexPre   = BS.take hexLen hex
          hexPost  = BS.drop hexLen hex
          hexPad s = if BS.length s == 0
                        then "00"
                        else if even BS.length
                                then s
                                else BS.cons "0" s
