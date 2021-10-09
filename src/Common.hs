module Common where

import Control.Lens
import Reanimate
import Reanimate.Scene
import Reanimate.Svg
import Graphics.SvgTree

moveTopLeft :: Double -> Object s a -> Scene s ()
moveTopLeft scaleFactor obj = do
  oTweenS obj 1 $ \t -> do
    oScale %= \prev -> fromToS prev scaleFactor t
    oLeftX %= \prev -> fromToS prev screenLeft t
    oTopY %= \prev -> fromToS prev screenTop t

-- WARN: Transformations (e.g. ) are applied using mkGroup
-- Still works for our use case but beware
removeOuterGroup :: SVG -> [SVG]
removeOuterGroup (GroupTree g)
  | g ^. drawAttributes == defaultSvg = concatMap dropNulls (g ^. groupChildren)
  | otherwise = fmap (\t -> t & drawAttributes .~ g ^. drawAttributes) (g ^. groupChildren)
  where
    dropNulls None = []
    dropNulls (DefinitionTree d)
      | null (d^.groupChildren) = []
    dropNulls (GroupTree g)
      | null (g^.groupChildren) = []
    dropNulls t = [t]
removeOuterGroup _ = []

objectToSvg :: Object s SVG -> Scene s SVG
objectToSvg o = do
  svg <- oRead o oSVG
  f <- oRead o oContext
  return $ f svg

objectsToSvg :: [Object s SVG] -> Scene s SVG
objectsToSvg objs =  mkGroup <$> mapM objectToSvg objs

glyphsCenterY :: [SVG] -> Double
glyphsCenterY glyphs =
  let allHeights = svgHeight <$> glyphs
      totalHeight = sum allHeights
   in totalHeight / fromIntegral (length glyphs * 2)

yCoordinates :: Double -> [SVG] -> [Double]
yCoordinates initY classGlyphs =
  let allHeights = svgHeight <$> classGlyphs
   in foldr (\h acc -> acc ++ [last acc - h]) [initY] allHeights

mkClass :: Double -> SVG -> [SVG] -> SVG
mkClass leftX header sigs =
  mkGroup $ mkClassParts leftX header sigs

mkClassParts :: Double -> SVG -> [SVG] -> [SVG]
mkClassParts leftX header sigs =
  let headerLeftX = boundingBox header ^. _1
      xOffsets = fmap (\i -> leftX - boundingBox i ^. _1) sigs --TODO: Make with lenses
      initY = glyphsCenterY (header:sigs)
      yOffsets = fmap (\(e, y) -> y - boundingBox e ^. _2 + svgHeight e) (zip (header:sigs) (yCoordinates initY (header:sigs)))
      zipped = zip3 (leftX - headerLeftX - 0.5:xOffsets) yOffsets (header:sigs)
      translated = map (\(x, y, glyph) -> translate x y glyph) zipped
   in translated

topYCoords :: [SVG] -> [Double]
topYCoords = yCoordinates screenTop
