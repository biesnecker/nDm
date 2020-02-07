{-# LANGUAGE RecordWildCards #-}

module Parse
  ( parseRollSpec
  ) where

import           Control.Applicative          ((<|>))
import           Data.Char                    (isDigit)
import           Text.ParserCombinators.ReadP
import           Types                        (Modifier (..), RollSpec (..))

consumeD :: ReadP ()
consumeD = do
  satisfy $ \c -> c == 'D' || c == 'd'
  return ()

consumeX :: ReadP ()
consumeX = do
  satisfy $ \c -> c == 'X' || c == 'x'
  return ()

parseInt :: ReadP Int
parseInt = do
  i <- many1 $ satisfy isDigit
  return $ read i

parseModifier :: ReadP Modifier
parseModifier = parseAdd <|> parseMinus
  where
    parseAdd = do
      satisfy (== '+')
      Plus <$> parseInt
    parseMinus = do
      satisfy (== '-')
      Minus <$> parseInt

parseRollCount :: ReadP Int
parseRollCount = do
  consumeX
  parseInt

singleNumberParser :: ReadP RollSpec
singleNumberParser = do
  n <- parseInt
  eof
  return $ nD6RollSpec n

fullStringParser :: ReadP RollSpec
fullStringParser = do
  n <- parseInt
  consumeD
  m <- parseInt
  modi <- option NoModifier parseModifier
  rc <- option 1 parseRollCount
  eof
  return $ RollSpec n m modi rc

nD6RollSpec :: Int -> RollSpec
nD6RollSpec n = RollSpec {..}
  where
    m = 6
    modifier = NoModifier
    rollCount = 1

parseRollSpec :: String -> Maybe RollSpec
parseRollSpec s =
  case parse s of
    [(r, "")] -> Just r
    _         -> Nothing
  where
    parse = readP_to_S prsr
    prsr = singleNumberParser <|> fullStringParser
