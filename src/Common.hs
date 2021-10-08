module Common where

import Control.Lens ((%=), (&), (%~), (^.), (.~), (^..), traverseOf)
import Reanimate (fromToS, screenLeft, screenTop, SVG, mkGroup)
import Reanimate.Scene (Object, Scene, oLeftX, oScale, oTopY, oTweenS, oRead, oSVG, oContext)
import Graphics.SvgTree
import Control.Lens.Combinators (each)

moveTopLeft :: Object s a -> Double -> Scene s ()
moveTopLeft obj scaleFactor = do
  oTweenS obj 1 $ \t -> do
    oScale %= \prev -> fromToS prev scaleFactor t
    oLeftX %= \prev -> fromToS prev screenLeft t
    oTopY %= \prev -> fromToS prev screenTop t

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
