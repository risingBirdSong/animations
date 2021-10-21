module Main where

import Reanimate
import Monad.Maybe.Common (showClass)
import qualified Monad.Maybe.Animation as MonadMaybe

main :: IO ()
main =
  reanimate MonadMaybe.animation
