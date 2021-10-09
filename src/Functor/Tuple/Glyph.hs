{-# LANGUAGE OverloadedStrings #-}

module Functor.Tuple.Glyph where

import Functor.Common ( scaleFactor )
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
import Control.Lens ((^.), _1)

bg :: SVG
bg = mkBackground "lightblue"

env :: Animation -> Animation
env = addStatic bg

space :: Double
space = 0.46

{-
 TODO: Using this split three parts for the signature since I couldn't figure
 how to use any other method so far:
 - Minted failed because it requires a -shell-escape flag but there's no current way to pass this flag to reanimate. Tested using the headers but that seemed to break ifplatform and the message about the missing flag persisted
 - Haven't tested listings
-}

fmapTupleSignature :: SVG
fmapTupleSignature = scale scaleFactor $ latex "fmap :: (a $\\rightarrow$ b) $\\rightarrow$ (a, a) $\\rightarrow$ (a, b)"

fmapTuple :: SVG
fmapTuple = scale scaleFactor $ latex "fmap f (x, y) = (x, f y)"

fmapFunctionTuple :: SVG
fmapFunctionTuple = center $ latex "fmap (* 3) (5, 2)"

fmap' :: SVG
fmap' =
  snd $
    splitGlyphs [0..3] fmapFunctionTuple

f :: SVG
f =
  snd $
    splitGlyphs [4..7] fmapFunctionTuple

oParen :: SVG
oParen =
  snd $
    splitGlyphs [8] fmapFunctionTuple

fstArg :: SVG
fstArg =
  snd $
    splitGlyphs [9] fmapFunctionTuple

comma :: SVG
comma =
  snd $
    splitGlyphs [10] fmapFunctionTuple

sndArg :: SVG
sndArg =
  snd $
    splitGlyphs [11] fmapFunctionTuple

cParen :: SVG
cParen =
  snd $
    splitGlyphs [12] fmapFunctionTuple

lFmapTuple :: [SVG]
lFmapTuple = [fmap', f, oParen, fstArg, comma, sndArg, cParen]

tuple :: [SVG]
tuple = [oParen, fstArg, comma, sndArg, cParen]

upToOpenParen :: Double
upToOpenParen =
  let (leftX, _, width, _) = boundingBox oParen
   in leftX + width

fullFunctionWidth :: SVG -> Double
fullFunctionWidth arg = svgWidth fmapFunctionTuple + svgWidth arg + space

tupleInstance :: SVG
tupleInstance = scale scaleFactor $ latex "instance Functor ((,) a) where"

preEvaluation :: SVG
preEvaluation = center $ latex "(5, (* 3) 2)"

pOne :: SVG
pOne =
  snd $
    splitGlyphs [0..2] preEvaluation

pTwo :: SVG
pTwo =
  snd $
    splitGlyphs [3..7] preEvaluation

pThree :: SVG
pThree =
  snd $
    splitGlyphs [8, 9] preEvaluation

pOParen :: SVG
pOParen = snd $ splitGlyphs [0] preEvaluation

pFst :: SVG
pFst = snd $ splitGlyphs [1] preEvaluation

pComma :: SVG
pComma = snd $ splitGlyphs [2] preEvaluation

pSnd :: SVG
pSnd = snd $ splitGlyphs [7] preEvaluation

pCParen :: SVG
pCParen = snd $ splitGlyphs [8] preEvaluation

xes :: [SVG] -> [Double]
xes = map (\x -> boundingBox x ^. _1)

evaluated :: SVG
evaluated = center $ latex "(5, 6)"

result :: SVG
result =
  snd $
    splitGlyphs [3] evaluated

endParen :: SVG
endParen =
  snd $
    splitGlyphs [4] evaluated
