{-# LANGUAGE OverloadedStrings #-}

module Zip.Scene where

import Zip.Glyph
import Common (moveTopLeft)
import Control.Lens ((%=), (.=))
import Control.Monad (forM, forM_)
import Reanimate
  ( SVG,
    fromToS,
    screenBottom,
    screenTop,
  )
import Reanimate.LaTeX (latex)
import Reanimate.Scene hiding (rectHeight, rectWidth)

showSignature :: Scene s (Object s SVG)
showSignature = do
  fullSignature <- oNew fullSig
  oShow fullSignature
  wait 1
  moveTopLeft 0.3 fullSignature
  return fullSignature

showNumbers :: Scene s [Object s SVG]
showNumbers = do
  -- Show list of numbers using FadeIn
  oL1 <- oNew l1
  oShowWith oL1 oFadeIn
  funcHeight <- oRead oL1 oBBHeight

  -- Move list of numbers to the top
  oTweenS oL1 1 $ \t -> do
    oTopY %= \prev -> fromToS prev (screenTop - funcHeight) t

  -- Replace the number list with a list of objects
  numbers <- mapM oNew numberList
  currentY <- oRead oL1 oTopY

  -- Set the Y value to the full list values
  forM_
    numbers
    ( \o -> do
        oModifyS o $ oTopY .= currentY
    )

  -- Show the number list
  mapM_ oShow numbers

  -- Hide the full list
  oHide oL1

  return numbers

showCharacters :: Object s SVG -> Scene s [Object s SVG]
showCharacters oL2 = do
  -- Create the objects for the character list glyphs
  characters <- mapM oNew characterList

  -- Get the bottom y coordinate of the second list
  currentY2 <- oRead oL2 oBottomY

  -- Update the bottom y coordinate of the characters
  forM_
    characters
    ( \o -> do
        oModifyS o $ oBottomY .= currentY2
    )

  mapM_ oShow characters

  return characters

showConcreteFirstList :: Object s SVG -> Scene s (Object s SVG)
showConcreteFirstList fullSignature = do
  -- Create object for the signature with the first list
  -- concrete type
  concreteFirstList <- oNew sigConcreteFirstList

  -- Get the original coordinates from the full signature
  originalLeftX <- oRead fullSignature oLeftX
  originalTopY <- oRead fullSignature oTopY

  -- Update the coordinates for the signature with the first list
  -- concrete type
  oModifyS concreteFirstList $ do
    oLeftX .= originalLeftX
    oTopY .= originalTopY

  -- Transform the fullSignature into the signature with
  -- the first list concrete type
  oTransform fullSignature concreteFirstList 0.4

  return concreteFirstList

showConcreteSecondList :: Object s SVG -> Object s SVG -> Scene s (Object s SVG)
showConcreteSecondList fullSignature concreteFirstList = do
  -- Get the original coordinates from the full signature
  originalLeftX <- oRead fullSignature oLeftX
  originalTopY <- oRead fullSignature oTopY

  oL2 <- oNew l2
  oShowWith oL2 oFadeIn
  oL2Height <- oRead oL2 oBBHeight

  -- Move the second list to the low part of the screen
  oTweenS oL2 1 $ \t -> oTopY %= \prev -> fromToS prev (screenBottom + (oL2Height * 3)) t

  -- Create the object from the signature with the second list
  -- concrete type
  concreteSecondList <- oNew sigConcreteSecondList

  -- Set the coordinates of the original signature
  oModifyS concreteSecondList $ do
    oLeftX .= originalLeftX
    oTopY .= originalTopY

  -- Transform to the signature with the concrete types
  oTransform concreteFirstList concreteSecondList 0.4
  return oL2

hideExtraElement :: Object s SVG -> Scene s ()
hideExtraElement dropped = oHideWith dropped oFadeOut

mkTuples :: [Object s SVG] -> [Object s SVG] -> Scene s [[Object s SVG]]
mkTuples characters numbers = do
  -- Zip the characters with the numbers
  let m = zip characters numbers
      -- Zip the tuples with initial uniformly positioned x coordiantes
      r = zip [-6, -3 .. 6] m
  forM r mkTuple

mkTuple :: (Double, (Object s SVG, Object s SVG)) -> Scene s [Object s SVG]
mkTuple (firstX, (character, number)) = do
  -- Get the width of the number
  numberWidth <- oRead number oBBWidth

  -- Left hand parenthesis
  lhp <- oNew $ latex "("
  -- Set the x coordinate based on the initial x positions
  oModifyS lhp $ oLeftX .= firstX - 2.3 >> oCenterY .= 0
  -- Show left hand paranthesis
  oShow lhp

  -- Move the number to the center
  oTweenS number 1 $ \t -> do
    oLeftX %= \prev -> fromToS prev (firstX - numberWidth - 1.5) t
    oCenterY %= \prev -> fromToS prev 0 t

  -- BottomY of the number (to align other objects with it)
  by <- oRead number oBottomY

  -- Comma
  comma <- oNew $ latex ","
  oModifyS comma $ oLeftX .= firstX - numberWidth - 0.8 >> oBottomY .= by - 0.2
  oShow comma

  -- Show the character
  oTweenS character 1 $ \t -> do
    oLeftX %= \prev -> fromToS prev (firstX - 0.7) t
    -- Set bottomY to the number's to align them
    oBottomY %= \prev -> fromToS prev by t

  -- Right hand parenthesis
  rp <- oNew $ latex ")"
  oModifyS rp $ oLeftX .= firstX >> oCenterY .= 0
  oShow rp

  -- Colon
  sc <- oNew $ latex ":"
  oModifyS sc $ oLeftX .= firstX + 0.4 >> oCenterY .= 0
  oShow sc

  -- Gather the different parts to move during evaluation
  return [sc, lhp, number, comma, character, rp]

showBrackets :: Object s SVG -> Scene s (Object s SVG)
showBrackets lastColon = do
  lastX <- oRead lastColon oLeftX
  -- Create an opening square bracket object
  lb <- oNew $ latex "["

  -- Create a closing square bracket object
  rb <- oNew $ latex "]"

  -- Set the coordinates for the brackets next to each other
  oModifyS lb $ oLeftX .= lastX + 0.2 >> oCenterY .= 0
  oModifyS rb $ oLeftX .= lastX + 0.3 >> oCenterY .= 0

  -- Show the brackets
  oShow lb
  oShow rb

  return lb

reduceFirstTuple :: [[Object s SVG]] -> Scene s (Object s SVG)
reduceFirstTuple tuples = do
    let lastColon = head $ last tuples
        lastTuple = drop 1 $ last tuples

    -- Show brackets based on the last colon
    lb <- showBrackets lastColon

    -- Get the x coordinate of the last element of the tuple sans the first element
    lastColonX <- oRead (head lastTuple) oLeftX

    -- Hide the left bracket and the last colon in parallel
    fork $ oHideWith lb oFadeOut
    oHideWith lastColon oFadeOut

    -- Stop the rest of the animation while we move the last tuple right
    waitOn $
      forM_
        lastTuple
        ( \e -> fork $ do
            x <- oRead e oLeftX
            oTweenS e 1 $ \t -> oLeftX %= \prev -> fromToS prev (x + 0.3) t
        )

    -- Update the left bracket with the position of the last colon
    -- and show it
    oModifyS lb $ oLeftX .= lastColonX
    fork $ oShowWith lb oFadeIn
    return lb

reduceTuples :: Object s SVG -> [[Object s SVG]] -> Scene s ()
reduceTuples lb tuples = forM_ tuples (reduceTuple lb)

reduceTuple :: Object s SVG -> [Object s SVG] -> Scene s ()
reduceTuple lb tuple = do
  -- Get the colon and the next tuple
  let colon = head tuple
      lTuple = drop 1 tuple

  -- Get the left x coordinate of the last colon
  lastColonX <- oRead (head lTuple) oLeftX
  -- Get the bottom y coordinate of the left hand parenthesis
  by <- oRead (lTuple !! 1) oBottomY
  -- Get the left x coordinate of the left bracket
  lbX <- oRead lb oLeftX
  -- Create a new comma object
  cm <- oNew $ latex ","
  -- Set the coordinates for the comma
  oModifyS cm $ oLeftX .= lbX >> oBottomY .= by - 0.2

  -- Hide the left bracket and the colon in parallel
  fork $ oHideWith lb oFadeOut
  oHideWith colon oFadeOut

  -- Move the rest of components to the right
  forM_
    lTuple
    ( \e -> fork $ do
        x <- oRead e oLeftX
        oTweenS e 1 $ \t -> oLeftX %= \prev -> fromToS prev (x + 0.2) t
    )

  -- Show the new comma
  oShowWith cm oFadeIn

  -- Set the new coordiantes of the left bracket
  oModifyS lb $ oLeftX .= lastColonX
  -- Show the bracket again
  oShowWith lb oFadeIn
