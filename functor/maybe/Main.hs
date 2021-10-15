module Main (main) where

import Reanimate
import Functor.Maybe.Animation (animation)

main :: IO ()
main = reanimate animation
