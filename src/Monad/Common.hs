{-# LANGUAGE OverloadedStrings #-}

module Monad.Common where

import Reanimate
import Control.Lens

scaleFactor :: Double
scaleFactor = 0.5

classHeader :: SVG
classHeader =
  scale scaleFactor $
    center $ latex "class Applicative m $\\Rightarrow$ Monad m where"

bindSignature :: SVG
bindSignature =
  scale scaleFactor $
    center $
      latex "(\\textgreater\\textgreater=) :: forall a b. m a $\\rightarrow$ (a $\\rightarrow$ m b) $\\rightarrow$ m b"

sequenceSignature :: SVG
sequenceSignature =
  scale scaleFactor $
    center $ latex "(\\textgreater\\textgreater) :: forall a b. m a $\\rightarrow$ m b $\\rightarrow$ m b"

returnSignature :: SVG
returnSignature =
  scale scaleFactor $
    center $ latex " return :: a $\\rightarrow$ m a"

baseX :: Double
baseX = boundingBox bindSignature ^. _1
