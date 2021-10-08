{-# LANGUAGE OverloadedStrings #-}

module Fold.Glyph where

import Control.Lens
import Reanimate
  ( Animation,
    SVG,
    addStatic,
    center,
    latex,
    mkBackground,
    scale,
    splitGlyphs,
    boundingBox
  )

-- Background
bg :: SVG
bg = mkBackground "lightblue"

-- Animation environment
env :: Animation -> Animation
env = addStatic bg

-- The general signature for foldr
signature :: SVG
signature =
  scale 0.6 $
    center $
      latex "Foldable t $\\Rightarrow$ foldr :: (a  $\\rightarrow$ b $\\rightarrow$ b) $\\rightarrow$ b $\\rightarrow$ t a $\\rightarrow$ a"

-- The concrete function to evaluate
concreteFunction :: SVG
concreteFunction = center $ latex "foldr (*) 1 [1..4]"

-- The concrete function with the range expanded
concreteRange :: SVG
concreteRange = center $ latex "foldr (*) 1 [1, 2, 3, 4]"

-- The generated redex after recursion
redex :: SVG
redex = center $ latex "1*(2*(3*(4*(1))))"

-- Glyph for the initial value provided to foldr
baseValue :: SVG
baseValue =
  snd $
    splitGlyphs [8] concreteRange

-- Glyph for the multiplication symbol
by :: SVG
by =
  snd $
    splitGlyphs [6] concreteRange

-- Glyph for the first element in the list to fold
one :: SVG
one =
  snd $ splitGlyphs [10] concreteRange

-- Glyph for the second element in the list to fold
two :: SVG
two =
  snd $ splitGlyphs [12] concreteRange

-- Glyph for the third element in the list to fold
three :: SVG
three =
  snd $ splitGlyphs [14] concreteRange

-- Glyph for the fourth element in the list to fold
four :: SVG
four =
  snd $ splitGlyphs [16] concreteRange

-- A left parent
leftParen :: SVG
leftParen = latex "("

-- All the argument in triples that contain the number, the multiplication symbol and the left parenthesis
args :: [(SVG, SVG, SVG)]
args = [(one, by, leftParen), (two, by, leftParen), (three, by, leftParen), (four, by, leftParen)]

fh :: Double
fh = boundingBox concreteRange ^. _4
