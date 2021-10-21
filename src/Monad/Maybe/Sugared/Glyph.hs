{-# LANGUAGE OverloadedStrings #-}

module Monad.Maybe.Sugared.Glyph where

import Reanimate ( latex, center, scale, SVG )
import Monad.Common ( scaleFactor )

f :: SVG
f =
  center $ scale scaleFactor $
    latex "sumMaybes mx my = do"

fMx :: SVG
fMx =
  center $ scale scaleFactor $
    latex "x $\\leftarrow$ mx"

fMy :: SVG
fMy =
  center $ scale scaleFactor $
    latex "y $\\leftarrow$ y"

fReturn :: SVG
fReturn =
  center $ scale scaleFactor $
    latex "return \\$ x + y"

fConcreteMx :: SVG
fConcreteMx =
  scale scaleFactor $
    latex "sumMaybes (Just 3) my = do"

fConcreteMy :: SVG
fConcreteMy =
  scale scaleFactor $
    latex "sumMaybes (Just 3) (Just 4) = do"

fMxConcrete :: SVG
fMxConcrete =
  scale scaleFactor $
    latex "3 $\\leftarrow$ Just 3"

fMyConcrete :: SVG
fMyConcrete =
  scale scaleFactor $
    latex "4 $\\leftarrow$ Just 4"

fReturnConcreteX :: SVG
fReturnConcreteX =
  scale scaleFactor $
    latex "return \\$ 3 + y"

fReturnConcreteY :: SVG
fReturnConcreteY =
  center $ scale scaleFactor $
    latex "return \\$ 3 + 4"

