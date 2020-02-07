{-# LANGUAGE RecordWildCards #-}

module Roll
  ( rollDice
  ) where

import           Control.Monad        (replicateM)
import           Control.Monad.Random
import           Parse                (parseRollSpec)
import           Types

dummyRoll :: Roll
dummyRoll = Roll 123 []

-- Roll 1dM
rollOne :: MonadRandom m => Int -> m Int
rollOne m = getRandomR (1, m)

-- Roll ndM
rollMany :: MonadRandom m => Int -> Int -> m [Int]
rollMany n m = replicateM n $ rollOne m

-- Add the modifier to a roll
calculateTotal :: Modifier -> [Int] -> Int
calculateTotal modi rolls =
  case modi of
    NoModifier -> s
    Plus i     -> s + i
    Minus i    -> s - i
  where
    s = sum rolls

genRoll ::  MonadRandom m => RollSpec -> m Roll
genRoll RollSpec {..} = do
  individual <- rollMany n m
  return $ Roll (calculateTotal modifier individual) individual

rollDiceImpl :: MonadRandom m => RollSpec -> m RollResult
rollDiceImpl rollSpec = do
  rolls <- replicateM (rollCount rollSpec) $ genRoll rollSpec
  return RollResult{..}

rollDice :: String -> IO (Maybe RollResult)
rollDice formatString =
  case parseRollSpec formatString of
    Nothing -> return Nothing
    Just rs -> Just <$> rollDiceImpl rs
