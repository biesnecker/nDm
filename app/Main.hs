{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad       (forM_)
import           Data.List           (intercalate)
import           Options.Applicative
import           Roll                (rollDice)
import           System.Exit         (exitFailure, exitSuccess)
import           Text.Printf         (printf)
import           Types

data Args =
  Args
    { formatString :: String
    , verbose      :: Bool
    }

formatHelp :: String
formatHelp =
  unwords
    [ "Either a single number, which will roll Nd6, or a full string nDm[+/-A][xB],"
    , "where the last two values are optional, and allow you to specify a fixed modifier"
    , "and the number of times to roll the combination. Examples:"
    , "3d6, 1d20+7, 2d6-2, 1d4+1x3, 5d8x9"
    ]

diceArgs :: Parser Args
diceArgs =
  Args <$> argument str (metavar "FORMAT" <> help formatHelp) <*>
  switch (long "verbose" <> short 'v' <> help "Print individual die rolls")

opts :: ParserInfo Args
opts =
  info
    (diceArgs <**> helper)
    (fullDesc <> progDesc "Roll dice based on format string" <> header "dice")

displaySimple :: Roll -> IO ()
displaySimple r = print $ total r

displayVerbose :: Roll -> IO ()
displayVerbose Roll {..} =
  putStrLn $
  show total ++ " [" ++ (intercalate ", " (map show individual)) ++ "]"

displayRollSpec :: RollSpec -> IO ()
displayRollSpec RollSpec {..} =
  putStrLn $ "Rolling " ++ roll ++ modi ++ " " ++ count
  where
    roll = printf "%dd%d" n m
    modi =
      case modifier of
        NoModifier -> ""
        Plus i     -> printf "+%d" i
        Minus i    -> printf "-%d" i
    count =
      case rollCount of
        1 -> "1 time"
        _ -> printf "%d times" rollCount

displayResults :: Args -> RollResult -> IO ()
displayResults Args {..} RollResult {..} =
  if verbose
    then do
      displayRollSpec rollSpec
      forM_ rolls displayVerbose
    else forM_ rolls displaySimple

main :: IO ()
main = do
  args@Args {..} <- customExecParser (prefs showHelpOnError) opts
  res <- rollDice formatString
  case res of
    Nothing -> do
      putStrLn $
        unwords
          [ "Failed to parse format string \""
          , formatString
          , "\", try --help for examples."
          ]
      exitFailure
    Just rolls -> do
      displayResults args rolls
      exitSuccess
