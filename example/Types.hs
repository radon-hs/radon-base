{-# LANGUAGE TemplateHaskell #-}

module Types where

import Radon.Access.Defaults ()
import Radon.Access.Gen (genAll)

data Vector2 = Vector2
  { x :: Float,
    y :: Float
  }
  deriving (Show, Eq)

data Person = Person
  { weight :: Float,
    favoriteNumbers :: (Int, Int, Int, Int),
    carModel :: Maybe String,
    name :: String
  }
  deriving (Show, Eq)

data MyStructure = MyStructure
  { position :: Vector2,
    velocity :: Vector2,
    person :: Person
  }
  deriving (Show, Eq)

$(genAll ''Vector2)
$(genAll ''Person)
$(genAll ''MyStructure)
