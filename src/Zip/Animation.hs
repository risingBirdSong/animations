{-# LANGUAGE OverloadedStrings #-}
module Zip.Animation where

import Zip.Glyph
import Zip.Scene
import Reanimate (Animation)
import Reanimate.Scene hiding (rectHeight, rectWidth)

animation :: Animation
animation = env $
  scene $ do
    fullSignature <- showSignature
    numbers <- showNumbers
    concreteFirstList <- showConcreteFirstList fullSignature
    oL2 <- showConcreteSecondList fullSignature concreteFirstList
    characters <- showCharacters oL2
    oHide oL2

    hideExtraElement (numbers !! 5)

    wait 0.2

    tuples <- mkTuples characters numbers
    lb <- reduceFirstTuple tuples

    -- Get the rest of the elements to animate after the first
    -- manual round
    let rest = reverse $ take (length tuples - 1) tuples
    reduceTuples lb rest
