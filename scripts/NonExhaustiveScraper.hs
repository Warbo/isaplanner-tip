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
import           Text.Parsec.ByteString.Lazy   (Parser)
import           Text.Parsec.Char
import           Text.Parsec.Combinator
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
      run testPattern,
      run testPatterns,
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

unmatchedPattern :: Parsec BS.ByteString _ Pattern
unmatchedPattern = do string "Global"
                      hex <- many1 (oneOf "0123456789abcdefABCDEF")
                      spaces
                      args <- char '_' `sepEndBy` spaces
                      return (Pat { patName = BS.append "Global" (BS.pack hex),
                                    patArgs = fromIntegral (length args) })

testPattern = ("can spot pattern", go)
  where go p = case parse unmatchedPattern "test" (renderPat p) of
          Left err -> error (show err)
          Right p' -> p === p'

patternsBlock :: Parsec BS.ByteString _ [Pattern]
patternsBlock = unmatchedPattern `sepEndBy1` spaces

testPatterns = ("can read block of patterns", go)
  where go (NonEmpty ps) =
          let i = BS.unlines (map renderPat ps)
              r = case parse patternsBlock "test" i of
                    Left err  -> error (show err)
                    Right ps' -> ps === ps'
           in counterexample (unlines ["BEGIN INPUT", BS.unpack i, "END INPUT"])
                             r

renderPat (Pat pN pA) = BS.unwords (pN : replicate (fromIntegral pA) "_")

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
             pats <- patternsBlock
             return (Msg { msgName = BS.pack fName, msgPats = pats })

  where filename = many1 (noneOf ":")
        line     = many1 digit
        column   = many1 digit

        funcName = char '`' *> many1 (noneOf "'") <* oneOf "'" <* char ':'

        -- Given [a, b, c] these match 'a:b:c:' and 'a b c '
        colonSep = seqSep (char ':')
        spaceSep = seqSep spaces
        seqSep s = \case
          []   -> return (error "No result from seqSep")
          p:ps -> p >> s >> seqSep s ps

testWarning = ("match warnings", go)
  where go m@(Msg name pats) =
          let i = input name pats
           in counterexample ("BEGIN INPUT\n" ++ BS.unpack i ++ "\nEND INPUT")
              (case parse warning "test" i of
                  Left err -> error (show err)
                  Right m' -> m === m')

        input :: BS.ByteString -> [Pattern] -> BS.ByteString
        input name pats = BS.unlines (pre name ++ map renderPat pats)

        pre :: BS.ByteString -> [BS.ByteString]
        pre name = ["test.hs:1:2: Warning:",
               "Pattern match(es) are non-exhaustive",
               BS.concat ["In an equation for `", name, "':"],
               "Patterns not matched:"]

data Message = Msg { msgName :: BS.ByteString, msgPats :: [Pattern] }
     deriving (Eq, Show)

data Pattern = Pat { patName :: BS.ByteString, patArgs :: Natural   }
     deriving (Eq, Show)

instance Arbitrary Message where
  arbitrary = Msg <$> genName <*> listOf1 arbitrary
    where genName = do Hex h <- arbitrary
                       return (BS.append "global" h)

  shrink (Msg name pats) = foldl mkMsg [] (shrink (hexName, pats))
    where mkMsg acc (Hex h, ps) = if null ps
                                     then acc
                                     else Msg (BS.append "global" h) ps : acc
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
                 return (Hex (BS.pack (if even (length hex)
                                          then hex
                                          else pad : hex)))
    where genHexit = elements "0123456789abcdefABCDEF"

  shrink (Hex h) = if hexLen > 2
                      then map (Hex . hexPad) [hexPre, hexPost]
                      else []
    where hexLen   = BS.length h `div` 2
          hexPre   = BS.take hexLen h
          hexPost  = BS.drop hexLen h
          hexPad s = if BS.length s == 0
                        then "00"
                        else if even (BS.length s)
                                then s
                                else BS.cons '0' s
