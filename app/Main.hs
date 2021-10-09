{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import qualified Fold as Fold
import Reanimate
import Applicative.Tuple.Animation as ApplicativeTuple

main :: IO ()
main = reanimate ApplicativeTuple.animation
