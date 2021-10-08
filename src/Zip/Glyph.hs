{-# LANGUAGE OverloadedStrings #-}

module Zip.Glyph where

import Reanimate
  ( Animation,
    SVG,
    addStatic,
    center,
    mkBackground,
    scale,
    svgGlyphs,
  )
import Reanimate.LaTeX (latex, latexWithHeaders)

-- Background
bg :: SVG
bg = mkBackground "lightblue"

env :: Animation -> Animation
env = addStatic bg

-- General signature
sig :: SVG
sig = latex "zip :: [a] $\\rightarrow$ [b] $\\rightarrow$ [(a, b)]"

-- Signature with the first list concrete type
sigConcreteFirstList :: SVG
sigConcreteFirstList =
  scale 0.3 $
    center $
      latexWithHeaders
        ["\\usepackage{color}"]
        "zip :: [$\\textcolor{green}{Int}$] $\\rightarrow$ [b] $\\rightarrow$ [(\\textcolor{green}{Int}, b)]"

-- Signature with the second list concrete type
sigConcreteSecondList :: SVG
sigConcreteSecondList =
  scale 0.3 $
    center $
      latexWithHeaders
        ["\\usepackage{color}"]
        "zip :: [\\textcolor{green}{Int}] $\\rightarrow$ [\\textcolor{blue}{Char}] $\\rightarrow$ [(\\textcolor{green}{Int}, \\textcolor{blue}{Char})]"

-- The centered full signature
fullSig :: SVG
fullSig = center sig

-- The first list argument
l1 :: SVG
l1 = center $ latex "1 2 3 4 5 6"

-- The list of glyphs form the first list argument
numberList :: [SVG]
numberList = fmap (\(f, _, svg) -> f svg) (svgGlyphs l1)

-- The seocnd list argument
l2 :: SVG
l2 = center $ latex "a b c d e"

-- The list of glyphs from the second list argument
characterList :: [SVG]
characterList = fmap (\(f, _, svg) -> f svg) (svgGlyphs l2)
