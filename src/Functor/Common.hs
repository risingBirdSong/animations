{-# LANGUAGE OverloadedStrings #-}

module Functor.Common where

import Reanimate ( SVG, latex, fromToS )
import Reanimate.Svg
import Reanimate.Scene
import Control.Lens ((%=), (.=), (^.), _1)
import Reanimate.Constants
import Control.Monad

scaleFactor :: Double
scaleFactor = 0.3

classHeader' :: SVG
classHeader' = center $ latex "class Functor f where"

baseX :: Double
baseX = boundingBox fmapSignature' ^. _1

fmapSignature' :: SVG
fmapSignature' = center $ latex "fmap :: (a $\\rightarrow$ b) $\\rightarrow$ f a $\\rightarrow$ f b"

leftMap' :: SVG
leftMap' = center $ latex "\\textless \\$ :: a $\\rightarrow$ f a $\\rightarrow$ f b"

classHeader :: SVG
classHeader =
  let leftX = boundingBox fmapSignature' ^. _1
      x = boundingBox classHeader' ^. _1
      y = svgHeight fmapSignature'
   in translate (leftX - x - 0.5) y classHeader'

fmapSignature :: SVG
fmapSignature = fmapSignature'

leftMap :: SVG
leftMap =
  let leftX = boundingBox fmapSignature' ^. _1
      x = boundingBox leftMap' ^. _1
      y = - svgHeight fmapSignature'
   in translate (leftX - x) y leftMap'

fmapHeight :: Double
fmapHeight = svgHeight (scale scaleFactor fmapSignature)

leftMapHeight :: Double
leftMapHeight = svgHeight (scale scaleFactor leftMap)

classMethodsX :: Double
classMethodsX = screenLeft + 0.5

showArgument :: Double
             -- ^ Distance to the open parenthesis
             -> ([Object s SVG] -> [Object s SVG])
             -- ^ Accessor to the objects that make the argument
             -> [Object s SVG]
             -- ^ All the objects in the function
             -> Scene s ()
showArgument distanceToOpenParen getArg objs = do
    forM_ objs $ \o -> do
      leftX <- oRead o oLeftX
      oModifyS o $ oLeftX .= leftX - distanceToOpenParen

    let oArg = getArg objs

    forM_ oArg (\o -> fork $ oShowWith o oFadeIn)
