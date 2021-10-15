module Main (main) where

import Reanimate
import Functor.Tuple.Animation (animation)

main :: IO ()
main = reanimate animation
