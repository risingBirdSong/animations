{-# LANGUAGE OverloadedStrings #-}

module Functor.Maybe.Glyph where

import Functor.Common
import Reanimate
  ( Animation,
    SVG,
    addStatic,
    center,
    latex,
    mkBackground,
    scale, splitGlyphs
  )
import Reanimate.Svg (svgWidth, boundingBox)

bg :: SVG
bg = mkBackground "lightblue"

env :: Animation -> Animation
env = addStatic bg

space :: Double
space = 0.46

maybeInstance :: SVG
maybeInstance = scale scaleFactor $ latex "instance Functor Maybe where"

fmapMaybeSignature :: SVG
fmapMaybeSignature = scale scaleFactor $
  center $
    latex "fmap :: (a $\\rightarrow$ b) $\\rightarrow$ Maybe a $\\rightarrow$ Maybe b"

fmapJust :: SVG
fmapJust = scale scaleFactor $ latex "fmap f (Just x) = Just (f x)"

fmapNothing :: SVG
fmapNothing = scale scaleFactor $ latex "fmap {\\_} Nothing = Nothing"

fmapFunctionMaybe :: SVG
fmapFunctionMaybe = center $ latex "fmap (* 3) (Just 5)"

fmapFunctionNothing :: SVG
fmapFunctionNothing = center $ latex "fmap (*3) Nothing"

fmap' :: SVG
fmap' =
  snd $
    splitGlyphs [0..3] fmapFunctionMaybe

f :: SVG
f =
  snd $
    splitGlyphs [4..7] fmapFunctionMaybe

oParen :: SVG
oParen =
  snd $
    splitGlyphs [8] fmapFunctionMaybe

cParen :: SVG
cParen =
  snd $
    splitGlyphs [14] fmapFunctionMaybe

argJust :: SVG
argJust =
  snd $
    splitGlyphs [9..12] fmapFunctionMaybe

argValue :: SVG
argValue =
  snd $
    splitGlyphs [13] fmapFunctionMaybe

lFmapMaybe :: [SVG]
lFmapMaybe = [argJust, argValue, fmap', f, oParen, cParen]

upToOpenParen :: Double
upToOpenParen =
  let (leftX, _, width, _) = boundingBox oParen
   in leftX + width + (svgWidth argJust / 2)

fullFunctionWidth :: SVG -> Double
fullFunctionWidth arg = svgWidth fmapFunctionMaybe + svgWidth arg + space
