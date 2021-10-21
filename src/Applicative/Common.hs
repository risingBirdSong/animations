{-# LANGUAGE OverloadedStrings #-}

module Applicative.Common where

import Reanimate
import Control.Lens

scaleFactor :: Double
scaleFactor = 0.6

baseX :: Double
baseX = boundingBox liftA2Signature ^. _1

classHeader :: SVG
classHeader =
  scale scaleFactor $
    center $ latex "class Applicative where"

pureSignature :: SVG
pureSignature =
  scale scaleFactor $
    center $ latex "pure :: a $\\rightarrow$ fa"

applySignature :: SVG
applySignature =
  scale scaleFactor $
    center $ latex "(\\textless*\\textgreater) :: f (a $\\rightarrow$ b) $\\rightarrow$ f a $\\rightarrow$ fb"

liftA2Signature :: SVG
liftA2Signature =
  scale scaleFactor $
    center $ latex "liftA2 :: (a $\\rightarrow$ b $\\rightarrow$ c) $\\rightarrow$ f a $\\rightarrow$ f b $\\rightarrow$ f c"
