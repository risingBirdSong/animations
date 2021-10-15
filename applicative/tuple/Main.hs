module Main (main) where

import Reanimate
import Applicative.Tuple.Animation as ApplicativeTuple

main :: IO ()
main = reanimate ApplicativeTuple.animation
