{-# LANGUAGE TemplateHaskell #-}
module Types where

import Foreign (Ptr)
import Radon.Access.Gen (genAll, genAllWith)
import Radon.Access.Config (capacityArrayAccessorConfig)
import Raylib.Types (Color, Texture)
import Radon.Access.Raylib ()

data Bunny = Bunny
  { px :: Float,
    py :: Float,
    sx :: Float,
    sy :: Float,
    color :: Color
  }
  deriving (Show, Eq)

genAll ''Bunny

data AppState = AppState
  { texBunny :: Ptr Texture,
    halfTexWidth :: Float,
    halfTexHeight :: Float,
    bunnies :: [Bunny]
  }
  deriving (Show, Eq)

genAllWith ''AppState (Nothing, [('bunnies, capacityArrayAccessorConfig [t|AppState|] [t|Bunny|] [t|Int|] 500000)]) id
