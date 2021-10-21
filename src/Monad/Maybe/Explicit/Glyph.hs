{-# LANGUAGE OverloadedStrings #-}

module Monad.Maybe.Explicit.Glyph where

import Reanimate
    ( SVG,
      latex,
      center,
      scale )

import Monad.Common ( scaleFactor )

f :: SVG
f =
  center $ scale scaleFactor $
    latex "sumMaybes mx my ="

fMx :: SVG
fMx =
  center $ scale scaleFactor $
    latex "mx \\textgreater \\textgreater = \\textbackslash x $\\rightarrow$"

fConcreteMx :: SVG
fConcreteMx =
  center $ scale scaleFactor $
    latex "sumMaybes (Just 3) my ="

fMxConcrete :: SVG
fMxConcrete =
  center $ scale scaleFactor $
    latex "Just 3 \\textgreater \\textgreater = \\textbackslash 3 $\\rightarrow$"

fMyConcrete :: SVG
fMyConcrete =
  scale scaleFactor $
    latex "Just 4 \\textgreater \\textgreater = \\textbackslash 4 $\\rightarrow$"

fReturnConcreteX :: SVG
fReturnConcreteX =
  scale scaleFactor $
    latex "return \\$ 3 + y"

fMy :: SVG
fMy =
  center $
    scale scaleFactor $
      latex "my \\textgreater \\textgreater = \\textbackslash y $\\rightarrow$"

fConcreteMy :: SVG
fConcreteMy =
  scale scaleFactor $ latex "sumMaybes (Just 3) (Just 4) ="

fReturn :: SVG
fReturn =
  center $ scale scaleFactor $
    latex "return \\$ x + y"

fConcreteReturn :: SVG
fConcreteReturn =
  scale scaleFactor $
    latex "return \\$ 3 + 4"

