module Types
  ( RollSpec(..)
  , Modifier(..)
  , Roll(..)
  , RollResult(..)
  ) where

data RollSpec =
  RollSpec
    { n         :: Int
    , m         :: Int
    , modifier  :: Modifier
    , rollCount :: Int
    }

data Modifier
  = Plus Int
  | Minus Int
  | NoModifier

data Roll =
  Roll
    { total      :: Int
    , individual :: [Int]
    }

data RollResult =
  RollResult
    { rollSpec :: RollSpec
    , rolls    :: [Roll]
    }
