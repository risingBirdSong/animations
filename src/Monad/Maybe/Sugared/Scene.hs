module Monad.Maybe.Sugared.Scene where

import Reanimate
import Reanimate.Scene
import Common
import Control.Monad
import Monad.Maybe.Common
import Monad.Maybe.Sugared.Glyph
import Control.Lens

fullScene :: Scene s ()
fullScene = do
  oDoF <- showSugaredFunction
  wait 2
  let [_, oDoFunction, oDoFunctionMx, oDoFunctionMy, oDoFunctionReturn] = oDoF
      toReplaceMx = [oDoFunction, oDoFunctionMx, oDoFunctionReturn]
  toReplaceMxWith <- replaceMx toReplaceMx
  wait 1
  let toReplaceMy = [head toReplaceMxWith, oDoFunctionMy, last toReplaceMxWith]
  toReplaceMyWith <- replaceMy toReplaceMy
  wait 3
  forM_ (toReplaceMxWith ++ toReplaceMyWith) (fork . (`oHideWith` oFadeOut))

showSugaredFunction :: Scene s [Object s SVG]
showSugaredFunction = do
  let partsWithIndentation = zip (0:replicate 3 1) [ f
                                                   , fMx
                                                   , fMy
                                                   , fReturn ]
  oDoF <- mapM oNew $ mkFunctionParts signature partsWithIndentation
  forM_ oDoF (fork . (`oShowWith` oFadeIn))
  return oDoF

replaceMx :: [Object s SVG] -> Scene s [Object s SVG]
replaceMx toReplaceMx = do
  toReplaceMxWith <- mapM oNew [ fConcreteMx
                               , fMxConcrete
                               , fReturnConcreteX ]

  forM_ (zip toReplaceMx toReplaceMxWith) $ \(o, r) -> do
    x <- oRead o oLeftX
    y <- oRead o oTopY

    oModifyS r $ do
      oLeftX .= x
      oTopY .= y

    fork $ oHideWith o oFadeOut
    oShowWith r oFadeIn

  return toReplaceMxWith

replaceMy :: [Object s SVG] -> Scene s [Object s SVG]
replaceMy toReplaceMy = do
  toReplaceMyWith <- mapM oNew [ fConcreteMy
                               , fMyConcrete
                               , fReturnConcreteY
                               ]

  forM_ (zip toReplaceMy toReplaceMyWith) $ \(o, r) -> do
    x <- oRead o oLeftX
    y <- oRead o oTopY

    oModifyS r $ do
      oLeftX .= x
      oTopY .= y

    fork $ oHideWith o oFadeOut
    oShowWith r oFadeIn

  return toReplaceMyWith
