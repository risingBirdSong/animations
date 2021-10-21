{-# LANGUAGE OverloadedStrings #-}

module Monad.Maybe.Common where

import Monad.Common
import Reanimate
    ( addStatic, latex, center, mkBackground, scale, Animation, SVG )
import Reanimate.Scene
import Common

bg :: SVG
bg = mkBackground "lightblue"

env :: Animation -> Animation
env = addStatic bg

signature :: SVG
signature =
  center $ scale scaleFactor $
    latex "sumMaybes :: Maybe Int $\\rightarrow$ Maybe Int $\\rightarrow$ Maybe Int"

showClass :: Scene s ()
showClass = do
  oMonadClass <- oNew $ mkClass baseX classHeader [ bindSignature
                                                  , sequenceSignature
                                                  , returnSignature ]
  oShow oMonadClass
  wait 2
  moveTopScaling scaleFactor oMonadClass
  wait 1
