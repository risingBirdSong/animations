{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import qualified Fold as Fold
import qualified Fold.Animation as Fold
import Reanimate
import qualified Zip.Animation as Zip
import Functor.Maybe.Animation as FunctorMaybe

main :: IO ()
main = reanimate Fold.animation
