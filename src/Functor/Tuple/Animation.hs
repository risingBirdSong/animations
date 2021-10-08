{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Functor.Tuple.Animation (animation) where

import Common
import Functor.Common
import Reanimate.Scene ( oNew, oShowWith, oFadeIn )
import Functor.Tuple.Glyph
import Functor.Tuple.Scene
import Reanimate

animation :: Animation
animation = env $
  scene $ do
    -- Show the class
    oFunctorClass <- oNew $ mkGroup [classHeader, fmapSignature, leftMap]
    oShowWith oFunctorClass oFadeIn
    wait 1
    -- Move the class to the top left corner
    moveTopLeft oFunctorClass scaleFactor
    wait 1
    -- Show the tuple argument
    objs <- mapM oNew lFmapTuple
    showArgument upToOpenParen (tail . tail) objs
    wait 1
    -- Expand the fmap function
    expandFmapFunction oFunctorClass objs
    wait 1
    -- Evaluate the function
    fmapEvaluation objs
