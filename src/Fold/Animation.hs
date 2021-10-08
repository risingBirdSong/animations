{-# LANGUAGE OverloadedStrings #-}

module Fold.Animation where

import Fold.Glyph
import Fold.Scene
import Reanimate

animation :: Animation
animation = env $
  scene $ do
    showFunction
    partial <- displayRedex
    oOne <- evaluateInnerRedex (head $ last partial)
    evaluate oOne partial
