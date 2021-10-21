module Common where

import Control.Lens
import Reanimate
import Reanimate.Scene
import Graphics.SvgTree hiding (rectHeight, rectWidth)

moveTopLeft :: Double -> Object s a -> Scene s ()
moveTopLeft scaleFactor obj = do
  oTweenS obj 1 $ \t -> do
    oScale %= \prev -> fromToS prev scaleFactor t
    oLeftX %= \prev -> fromToS prev screenLeft t
    oTopY %= \prev -> fromToS prev screenTop t

moveTopScaling :: Double -> Object s a -> Scene s ()
moveTopScaling scaleFactor obj = do
  oTweenS obj 1 $ \t -> do
    oScale %= \prev -> fromToS prev scaleFactor t
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
   in totalHeight / 2

yCoordinates :: Double -> [SVG] -> [Double]
yCoordinates initY classGlyphs =
  let allHeights = fmap svgHeight classGlyphs
      maxHeight = maximum allHeights
   in foldr (\h acc -> acc ++ [last acc - h - (maxHeight - h / 2)]) [initY] (reverse allHeights)

mkClass :: Double -> SVG -> [SVG] -> SVG
mkClass leftX header sigs =
  mkGroup $ mkClassParts leftX header sigs

yOffsets :: [(SVG, Double)] -> [Double]
yOffsets = fmap (\(e, y) -> y - (abs boundingBox e ^. _2))

mkClassParts :: Double -> SVG -> [SVG] -> [SVG]
mkClassParts leftX header sigs =
  let headerLeftX = boundingBox header ^. _1
      xOffsets = fmap (\i -> leftX - boundingBox i ^. _1) sigs --TODO: Make with lenses
      initY = glyphsCenterY (header:sigs)
      yOffs = yOffsets (zip (header:sigs) (yCoordinates initY (header:sigs)))
      zipped = zip3 (leftX - headerLeftX - 0.5:xOffsets) yOffs (header:sigs)
      translated = map (\(x, y, glyph) -> translate x y glyph) zipped
   in translated

-- | Make function centered by the signature
mkFunction :: SVG -> [(Double, SVG)] -> SVG
mkFunction signature body = mkGroup $ mkFunctionParts signature body

-- | Make function parts centered by the signature
mkFunctionParts :: SVG -> [(Double, SVG)] -> [SVG]
mkFunctionParts signature body =
  let signatureLeftX = boundingBox signature ^. _1
      f = signature:(snd <$> body)
      initY = glyphsCenterY f
      xOffs = fmap (\(i, e) -> signatureLeftX - boundingBox e ^. _1 + (i * 0.5)) body
      yOffs = yOffsets (zip f (yCoordinates initY f))
      zipped = zip3 (0:xOffs) yOffs f
   in map (\(x, y, glyph) -> translate x y glyph) zipped

topYCoords :: [SVG] -> [Double]
topYCoords = yCoordinates screenTop

-- | Add black bounding box
addBB :: SVG -> SVG
addBB svg =
  let rect = centerUsing svg $ mkRect (svgWidth svg) (svgHeight svg)
   in mkGroup [ rect, svg ]

-- | Add bounding box especifying the fill color
addBBColor :: String -> SVG -> SVG
addBBColor color svg =
  let rect = withFillColor color $ centerUsing svg $ mkRect (svgWidth svg) (svgHeight svg)
   in mkGroup [ rect, svg ]
