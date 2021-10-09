{-# LANGUAGE OverloadedStrings #-}

module Fold.Scene where

import Control.Lens
import Control.Monad (forM, forM_)
import Data.List (zip4)
import qualified Data.Text as T (pack)
import Reanimate
  ( SVG,
    center,
    fromToS,
    latex,
    screenTop, boundingBox
  )
import Reanimate.Scene hiding (rectHeight, rectWidth)
import Fold.Glyph
import Common

showFunction :: Scene s ()
showFunction = do
  -- Create a signature object
  sig <- oNew signature
  -- Show the signature object
  oShow sig
  -- Move signature to the top left corner
  moveTopLeft 0.8 sig

  -- Create the object for the concreteFunction
  cf <- oNew concreteFunction
  -- Create the object for the function with the concreteRange
  cr <- oNew concreteRange

  -- Fade in the concrete function
  oShowWith cf oFadeIn
  -- Transform the concrete function to the concrete range function
  oTransform cf cr 1

  -- Get the original leftX value from the redex object
  -- Move the concrete range function up just below the original signature
  oTweenS cr 1 $ \t -> oTopY %= \prev -> fromToS prev (screenTop - fh) t

displayRedex :: Scene s [[Object s SVG]]
displayRedex = do
  forM args $ \(arg, newBy, paren) -> do
    -- Create objects from svg components
    oArg <- oNew arg
    originalBy <- oNew by
    oBy <- oNew newBy
    oParen <- oNew paren

    -- Get objects left x
    ax <- oRead oArg oLeftX

    -- Set the Y coordinate to the original place
    -- in the concrete range function's list
    oModifyS oArg $ oTopY .= screenTop - fh
    -- The parent must be centered in the y axis
    oModifyS oParen $ oCenterY .= screenTop - fh

    -- Show the number of the list overlapping with
    -- the original number in the concrete range function's list
    oShow oArg
    let concreteY = screenTop - fh

    -- The x offset is leftX of the full redex centered
    -- in the screen plus the leftX of the argument
    let xOffset = ax + (boundingBox redex ^. _1)

    -- Move start move the number to its place in the center
    -- of the screen (where they would normally appear if the
    -- full redex was shown using the center combinator
    oTweenS oArg 1 $ \t -> do
      oLeftX %= \prev -> fromToS prev xOffset t
      oCenterY %= \prev -> fromToS prev 0 t

    -- Get the leftX value of the concrete range function's
    -- multiplication symbol
    originalByX <- oRead originalBy oLeftX

    -- Set the multiplication symbol in the triple to the
    -- coordinates of the symbol of the concrete range function
    oModifyS oBy $ do
      oLeftX .= originalByX
      oTopY .= concreteY

    -- Show the multiplication symbol overlapping with
    -- the original symbol in the concrete range function's list
    oShow oBy

    -- Move the multiplication symbol next to the current number
    oTweenS oBy 0.6 $ \t -> do
      oLeftX %= \prev -> fromToS prev (xOffset + 0.5) t
      oCenterY %= \prev -> fromToS prev 0 t

    -- Set the opening parenthesis in the triple next to the multiplication
    -- operator
    oModifyS oParen $ do
      oLeftX .= xOffset + 0.9
      oCenterY .= 0

    -- Show the opening parenthesis
    oShowWith oParen oFadeIn

    -- We gather the objects created for the opening parenthesis, the number
    -- and the multiplication symbol
    return [oParen, oArg, oBy]

evaluateInnerRedex :: Object s SVG -> Scene s (Object s SVG)
evaluateInnerRedex lastParen = do
  -- Get the rightX of the last open parenthesis
  lastRightX <- oRead lastParen oRightX

  -- Create a one object
  oOne <- oNew $ center $ latex "1"

  -- Create an object form the glyph for the initial value in the concrete range
  -- function
  oBaseValue <- oNew baseValue

  -- Get the leftX value of the initial value
  baseValueX <- oRead oBaseValue oLeftX

  let concreteY = screenTop - fh

  -- Set the new oOne object to the coordinates of the base value
  -- in the concrete range function
  oModifyS oOne $ do
    oLeftX .= baseValueX
    oTopY .= concreteY

  -- Show the oOne overlapping the initial value
  oShow oOne

  -- Move the initial value next to the last opening parenthesis
  oTweenS oOne 1 $ \t -> do
    oCenterY %= \prev -> fromToS prev 0 t
    oCenterX %= \prev -> fromToS prev lastRightX t

  return oOne

addClosingParens :: Object s SVG -> Scene s [Object s SVG]
addClosingParens oOne = do
  -- Create list of the positions of the closing parenthesis relative to the rightX
  -- of the number oOne
  oneX <- oRead oOne oRightX
  let xs = [oneX, oneX + 0.5, oneX + 1, oneX + 1.5]

  -- Show the closing parenthesis in parallel
  mapM
    ( \x -> fork $ do
        paren <- oNew $ center $ latex ")"
        oModifyS paren $ oCenterX .= x
        oShowWith paren oFadeIn
        return paren
    )
    xs

reduceStep :: [Object s SVG]
           -> [[Object s SVG]]
           -> (Object s SVG, [Object s SVG], Object s SVG, Object s SVG)
           -> Scene s ()
reduceStep closingParens partial (byProduct, els, closingParen, toHide) = do
  -- Pattern match on the argument objects
  let [openParen, oldArg, oBy] = els

  oHide oBy
  oHide openParen
  oHide oldArg
  oHide closingParen
  oHide toHide

  -- Get the rightX of the current open parenthesis
  newArgX <- oRead openParen oRightX
  -- Set the new value's X to the right side of the
  -- open parenthesis
  oModifyS byProduct $ oRightX .= newArgX + 0.5
  -- Show the result of the current evaluation
  oShow byProduct

  -- Move the parenthesis to the left
  forM_ closingParens $ \p -> do
    currentX <- oRead p oLeftX
    fork $ oTweenS p 0 $ \t -> oLeftX %= \prev -> fromToS prev (currentX - 0.5) t

  -- Move the rest of the components to the right
  forM_ partial $ \os -> do
    forM_ os $ \o -> do
      currentX <- oRead o oLeftX
      fork $ oTweenS o 0 $ \t -> oLeftX %= \prev -> fromToS prev (currentX + 0.6) t

  wait 1

evaluate :: Object s SVG -> [[Object s SVG]] -> Scene s ()
evaluate oOne partial = do
  -- Add the closing parens to the redex
  closingParens <- addClosingParens oOne
  wait 1

  fork $ oHideWith (head $ last partial) oFadeOut
  fork $ oHideWith oOne oFadeOut

  oHideWith (last $ last partial) oFadeOut

  let byProducts = reverse $ init $ foldr (\x acc -> (head acc * x) : acc) [1] [1 .. 4]
      oFour = last partial !! 1 -- Get the oFour value

  oByProducts <- mapM (oNew . center . latex . T.pack . (show :: Integer -> String)) byProducts

  let objectsToHide = oFour : oByProducts

  forM_ (zip4 oByProducts (reverse partial) (reverse closingParens) objectsToHide)
        (reduceStep closingParens partial)
  wait 1

