{-# LANGUAGE OverloadedStrings #-}

module Functor.Maybe.Scene where

import Functor.Common
import Control.Lens ((%=), (.=), (^.), _1, _3)
import Control.Monad (forM_)
import Reanimate.Scene
import Reanimate.Animation
import Reanimate
import Functor.Maybe.Glyph
import Common (objectsToSvg)

expandFmapFunction :: Object s SVG -> [Object s SVG] -> Scene s (Object s SVG, Object s SVG)
expandFmapFunction oFunctorClass objs = do
  let oJust = head objs
      oArg = head $ tail objs
      rest = tail $ tail objs

  currentX <- oRead oArg oLeftX
  currentX' <- oRead oJust oLeftX

  fork $ oTweenS oArg 1 $ \t -> oLeftX %= \prev -> fromToS prev (currentX + upToOpenParen) t
  oTweenS oJust 1 $ \t -> oLeftX %= \prev -> fromToS prev (currentX' + upToOpenParen) t

  forM_ rest $ \o -> fork $ do
    leftX <- oRead o oLeftX
    oModifyS o $ oLeftX .= leftX + upToOpenParen
    oShowWith o oFadeIn

  oMaybeInstance <- oNew maybeInstance

  oModifyS oMaybeInstance $ do
    oLeftX .= screenLeft
    oTopY .= screenTop

  oFmapMaybeSig <- oNew fmapMaybeSignature

  oModifyS oFmapMaybeSig $ do
    oLeftX .= classMethodsX
    oTopY .= screenTop - fmapHeight

  let fmapJustHeight = svgHeight fmapJust

  oFmapJust <- oNew fmapJust

  oModifyS oFmapJust $ do
    oLeftX .= classMethodsX
    oTopY .= screenTop - fmapHeight - leftMapHeight - 0.1

  oFmapNothing <- oNew fmapNothing

  oModifyS oFmapNothing $ do
    oLeftX .= classMethodsX
    oTopY .= screenTop - fmapHeight - leftMapHeight - fmapJustHeight - 0.15

  fork $ oHideWith oFunctorClass oFadeOut
  fork $ oShowWith oMaybeInstance oFadeIn
  fork $ oShowWith oFmapMaybeSig oFadeIn
  fork $ oShowWith oFmapJust oDraw

  oShowWith oFmapNothing oDraw
  oModifyS oFmapJust $ oContext .= withFillColor "blue"

  return (oFmapJust, oFmapNothing)

fmapEvaluation :: Object s SVG -> Object s SVG -> [Object s SVG] -> Scene s ()
fmapEvaluation oFmapJust oFmapNothing objs=  do
    let aproxWidth = svgWidth argJust + svgWidth oParen + svgWidth f + svgWidth argValue + svgWidth cParen
        [oJust, oArg, oFmap', oF, oOParen, oCParen] = objs
        rest = drop 2 objs

    oJustWidth <- oRead oJust oBBWidth
    oOParenWidth <- oRead oOParen oBBWidth

    -- Hide the fmap
    fork $ oHideWith oFmap' oFadeOut
    fork $ oTweenS oJust 1 $ \t -> do
      oLeftX %= \prev -> fromToS prev (- (aproxWidth / 2) - 0.4) t
    fork $ oTweenS oOParen 1 $ \t->
      oLeftX %= \prev -> fromToS prev ( - (aproxWidth / 2) + oJustWidth + 0.2) t
    oTweenS oF 1 $ \t ->
      oLeftX %= \prev -> fromToS prev (- (aproxWidth / 2) + oJustWidth + oOParenWidth + 0.4) t

    wait 0.8

    oResult <- oNew $ center $ latex "15"
    oResultWidth <- oRead oResult oBBWidth

    oModifyS oResult $ do
      oLeftX .= (- (aproxWidth / 2) + oJustWidth + oOParenWidth + 0.2)

    fork $ oHideWith oF oFadeOut
    fork $ oHideWith oArg oFadeOut
    fork $ oTweenS oCParen 1 $ \t ->
      oLeftX %= \prev ->
        fromToS prev ( - (aproxWidth / 2) + oJustWidth + oOParenWidth + oResultWidth + 0.2) t

    oShowWith oResult oFadeIn

    oJustBottomY <- oRead oJust oBottomY

    forM_ rest $ \o -> fork $ oHideWith o oFadeOut
    fork $ oTweenS oJust 1 $ \t -> oLeftX %= \prev -> fromToS prev (- (oJustWidth + 0.2)) t
    oTweenS oResult 1 $ \t -> do
      oLeftX %= \prev -> fromToS prev 0.2 t
      oBottomY %= \prev -> fromToS prev oJustBottomY t -- TODO: set the right Y coordiante to align with the bottom of this

    wait 1.5

    oModifyS oFmapJust $ oContext .= withFillColor "black"

    fork $ oHideWith oResult oFadeOut
    oHideWith oJust oFadeOut

    oFNothing <- oNew fmapFunctionNothing
    oShowWith oFNothing oFadeIn

    wait 1

    oModifyS oFmapNothing $ oContext .= withFillColor "blue"

    oNothing <- oNew $ center $ latex "Nothing"

    fork $ oHideWith oFNothing oFadeOut
    oShowWith oNothing oFadeIn
