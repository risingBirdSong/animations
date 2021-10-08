{-# LANGUAGE OverloadedStrings #-}

module Functor.Tuple.Scene where

import Functor.Common
import Control.Lens ((%=), (.=), (^.), _1, _3)
import Control.Monad (forM_)
import Reanimate.Scene
import Reanimate.Animation
import Reanimate
import Functor.Tuple.Glyph
import Common (objectsToSvg)

expandFmapFunction :: Object s SVG -> [Object s SVG] -> Scene s ()
expandFmapFunction oFunctorClass objs = do
  let oTuple = tail $ tail objs
      rest = [head objs, head $ tail objs]

  currentTupleX <- mapM (`oRead` oLeftX) oTuple

  forM_ (zip oTuple currentTupleX) $ \(o, currentX) -> do
    fork $ oTweenS o 1 $ \t -> oLeftX %= \prev -> fromToS prev (currentX + upToOpenParen) t

  forM_ rest $ \o -> fork $ do
    leftX <- oRead o oLeftX
    oModifyS o $ oLeftX .= leftX + upToOpenParen
    oShowWith o oFadeIn

  oTupleInstance <- oNew tupleInstance

  oModifyS oTupleInstance $ do
    oLeftX .= screenLeft
    oTopY .= screenTop

  oFmapTupleSig <- oNew fmapTupleSignature

  oModifyS oFmapTupleSig $ do
    oLeftX .= classMethodsX
    oTopY .= screenTop - fmapHeight

  oFmapTuple <- oNew fmapTuple

  oModifyS oFmapTuple $ do
    oLeftX .= classMethodsX
    oTopY .= screenTop - fmapHeight - leftMapHeight - 0.1

  fork $ oHideWith oFunctorClass oFadeOut
  fork $ oShowWith oTupleInstance oFadeIn
  fork $ oShowWith oFmapTupleSig oFadeIn

  oShowWith oFmapTuple oDraw
  oModifyS oFmapTuple $ oContext .= withFillColor "blue"


fmapEvaluation :: [Object s SVG] -> Scene s ()
fmapEvaluation objs = do
    -- TODO: This isn't right
    let [oFmap', oF, oOParen, oFstArg, oComma, oSndArg, oCParen] = objs

    -- Hide the fmap
    fork $ oHideWith oFmap' oFadeOut

    let firstPartObjects = [oOParen, oFstArg, oComma]

    -- TODO: This thing doesn't work, crap, re-visit
    forM_ (zip firstPartObjects (xes [pOParen, pFst, pComma])) $ \(o, theX) -> do
      fork $ oTweenS o 1 $ \t -> oLeftX %= \prev -> fromToS prev theX t

    let fx = boundingBox pTwo ^. _1
    oTweenS oF 1 $ \t ->
      oLeftX %= \prev ->
        fromToS prev fx t

    forM_ (zip [oSndArg, oCParen] (xes [pSnd, pCParen])) $ \(o, tx) -> do
      fork $ oTweenS o 1 $ \t -> oLeftX %= \prev -> fromToS prev tx t

    wait 2

    oResult <- oNew $ center $ latex "6"
    width <- oRead oResult oBBWidth

    leftX <- oRead oF oLeftX

    oModifyS oResult $ do
      oLeftX .= leftX

    oFirstPart <- oNew =<< objectsToSvg firstPartObjects
    currentFirstPartX <- oRead oOParen oLeftX
    oModifyS oFirstPart $ oLeftX .= currentFirstPartX

    oShow oFirstPart

    forM_ firstPartObjects oHide

    fork $ oHideWith oF oFadeOut
    fork $ oHideWith oSndArg oFadeOut
    fork $ oTweenS oFirstPart 1 $
      \t -> oLeftX %= \prev -> fromToS prev (boundingBox evaluated ^. _1) t
    fork $ oTweenS oResult 1 $
      \t -> oLeftX %= \prev -> fromToS prev (boundingBox result ^. _1) t
    fork $ oTweenS oCParen 1 $
      \t -> oLeftX %= \prev -> fromToS prev (boundingBox endParen ^. _1) t

    oShowWith oResult oFadeIn

    wait 1.5
