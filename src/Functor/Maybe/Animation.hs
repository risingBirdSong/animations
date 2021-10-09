{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Functor.Maybe.Animation where

import Common
import Control.Lens ((%=), (.=))
import Reanimate
  ( Animation,
    SVG,
    addStatic,
    center,
    fromToS,
    latex,
    mkBackground,
    scale,
    screenTop,
    screenLeft, splitGlyphs, svgHeight, withFillColor, mkGroup
  )
import Reanimate.Scene
    ( fork,
      scene,
      wait,
      oBBHeight,
      oBBWidth,
      oCenterY,
      oLeftX,
      oModifyS,
      oNew,
      oRead,
      oShow,
      oTweenS, oShowWith, oFadeIn, oHideWith, oFadeOut, oTopY, oBottomY, oDraw, oContext )
import Reanimate.Svg (svgWidth, boundingBox)
import Control.Monad (forM_)

import Functor.Common
import Functor.Maybe.Glyph
import Functor.Maybe.Scene (expandFmapFunction, fmapEvaluation)

animation :: Animation
animation = env $
  scene $ do
    -- Show the class
    oFunctorClass <- oNew $ mkGroup [classHeader, fmapSignature, leftMap]
    oShowWith oFunctorClass oFadeIn
    wait 1
    -- Move the class to the top left corner
    moveTopLeft scaleFactor oFunctorClass
    wait 1
    -- Show the tuple argument
    objs <- mapM oNew lFmapMaybe
    showArgument upToOpenParen (take 2) objs
    wait 1
    -- Expand the fmap function
    (oFmapJust, oFmapNothing) <- expandFmapFunction oFunctorClass objs
    wait 1
    -- Evaluate the function
    fmapEvaluation oFmapJust oFmapNothing objs
    wait 3
