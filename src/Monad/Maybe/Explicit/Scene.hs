module Monad.Maybe.Explicit.Scene where

import Control.Monad
import Reanimate
import Reanimate.Scene
import Common
import Monad.Maybe.Common
import Monad.Maybe.Explicit.Glyph
import Control.Lens

fullScene :: Scene s ()
fullScene = do
    oExplicitF <- showExplicitFunction

    wait 2

    let [oExplicitSignature, oExplicitFunction, oExplicitFunctionMx, oExplicitFunctionMy, oExplicitFunctionReturn ] = oExplicitF
        toReplaceMx = [oExplicitFunction, oExplicitFunctionMx, oExplicitFunctionReturn]

    toReplaceMxWith <- replaceMx toReplaceMx

    wait 1

    let toReplaceMy = [head toReplaceMxWith, oExplicitFunctionMy, last toReplaceMxWith]

    toReplaceMyWith <- replaceMy toReplaceMy

    wait 3

    forM_ (oExplicitSignature:toReplaceMxWith ++ toReplaceMyWith) (fork . (`oHideWith` oFadeOut))

showExplicitFunction :: Scene s [Object s SVG]
showExplicitFunction = do
    oExplicitF <- mapM oNew $ mkFunctionParts signature (zip [0..3] [ f
                                                                    , fMx
                                                                    , fMy
                                                                    , fReturn ])

    forM_ oExplicitF (fork . (`oShowWith` oFadeIn))
    return oExplicitF

replaceMx :: [Object s SVG] -> Scene s [Object s SVG]
replaceMx toReplaceMx = do
  toReplaceMxWith <- mapM oNew [ fConcreteMx
                               , fMxConcrete
                               , fReturnConcreteX
                               ]

  forM_ (zip toReplaceMx toReplaceMxWith) $ \(o, r) -> do
    x <- oRead o oLeftX
    y <- oRead o oTopY

    oModifyS r $ do
      oLeftX .= x
      oTopY .= y

    fork $ oHide o
    oTransform o r 0.5

  return toReplaceMxWith

replaceMy :: [Object s SVG] -> Scene s [Object s SVG]
replaceMy toReplaceMy = do
    toReplaceMyWith <- mapM oNew [ fConcreteMy
                                 , fMyConcrete
                                 , fConcreteReturn
                                 ]

    forM_ (zip toReplaceMy toReplaceMyWith) $ \(o, r) -> do
      x <- oRead o oLeftX
      y <- oRead o oTopY

      oModifyS r $ do
        oLeftX .= x
        oTopY .= y

      oTransform o r 1

    return toReplaceMyWith
