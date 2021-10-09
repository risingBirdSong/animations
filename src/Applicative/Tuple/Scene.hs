module Applicative.Tuple.Scene where

import Reanimate ( SVG, Scene, fromToS, fork, wait, withFillColor )
import Applicative.Common ( baseX )
import Reanimate.Scene
    ( Object,
      oContext,
      oFadeIn,
      oFadeOut,
      oHideWith,
      oLeftX,
      oModifyS,
      oNew,
      oRead,
      oShow,
      oShowWith,
      oTopY,
      oTweenS )
import Control.Lens ( (%=), (.=) )

import Applicative.Tuple.Glyph
    ( tupleInstanceHeader,
      pureTuple,
      applyTuple,
      liftA2Tuple,
      pureFunctionGlyphs,
      pureMemptyArg,
      pureMemptyGlyphs,
      pureResultGlyphs,
      applyFunctionGlyphs,
      applyRedexGlyphs,
      applyResultGlyphs,
      liftA2Glyphs,
      lA2RedexGlyphs,
      lA2ResultGlyphs )
import Control.Monad ( forM_ )
import Common ( mkClassParts, topYCoords )

showClass :: Object s SVG -> Scene s ()
showClass oApplicativeClass = do
  oShowWith oApplicativeClass oFadeIn

pureScene :: Object s SVG -> Scene s [Object s SVG]
pureScene oApplicativeClass = do
  oPureFunction <- mapM oNew pureFunctionGlyphs
  let [oPurePrev, oPureArg, oPureRest] = oPureFunction
      instanceMethods = [pureTuple, applyTuple, liftA2Tuple]
  forM_ oPureFunction (fork . (`oShowWith` oFadeIn))

  wait 1

  oInstanceParts <- mapM oNew $ mkClassParts baseX tupleInstanceHeader instanceMethods
  let [_, oPureInstance, _, _] = oInstanceParts

  -- Get the class current leftX
  classX <- oRead oApplicativeClass oLeftX

  -- Get the y coordinates for the instance parts starting from screenTop
  let ys = topYCoords (tupleInstanceHeader:instanceMethods)
      xs = classX:replicate 3 (classX + 0.5)

  -- Set the right coordinates to superpoose with the class
  -- and show it
  fork $ oHideWith oApplicativeClass oFadeOut
  forM_ (zip3 oInstanceParts xs ys) $ \(o, x, y) -> do
    oModifyS o $ do
      oLeftX .= x
      oTopY .= y
    fork $ oShowWith o oFadeIn

  wait 1

  -- Highlight the pure method
  oModifyS oPureInstance $ oContext .= withFillColor "blue"

  wait 1

  -- Show the redex
  oPureFunctionMempty <- mapM oNew pureMemptyGlyphs

  let [oPureMemptyOParen, oPureMemptyRest, oPureMemptyComma, oPureMemptyCParen] = oPureFunctionMempty

  memptyArgLeftX <- (`oRead` oLeftX) =<< oNew pureMemptyArg

  forM_ [oPurePrev, oPureRest] (fork . (`oHideWith` oFadeOut))
  fork $ oTweenS oPureArg 1 $ \t -> oLeftX %= \prev -> fromToS prev memptyArgLeftX t
  forM_ [oPureMemptyOParen, oPureMemptyRest, oPureMemptyComma, oPureMemptyCParen] (fork . (`oShowWith` oFadeIn))

  wait 1

  -- Show the result
  oPureResult <- mapM oNew pureResultGlyphs

  let [oPureResultOParen, oPureResultFst, oPureResultComma, oPureResultArg, oPureResultCParen] = oPureResult
  newCoords <- mapM (`oRead` oLeftX) [ oPureResultOParen
                                     , oPureResultComma
                                     , oPureResultArg
                                     , oPureResultCParen ]
  fork $ oHideWith oPureMemptyRest oFadeOut

  wait 1

  let finalParts = [oPureMemptyOParen, oPureMemptyComma, oPureArg, oPureMemptyCParen]
      oldWithCoords = zip finalParts newCoords
  forM_  oldWithCoords $ \(o, x) ->
    fork $ oTweenS o 1 $ \t -> oLeftX %= \prev -> fromToS prev x t
  oShowWith oPureResultFst oFadeIn

  wait 1

  forM_ (oPureResultFst:finalParts) (fork . (`oHideWith` oFadeOut))
  oModifyS oPureInstance $ oContext .= withFillColor "black"

  wait 1

  return oInstanceParts

applyScene :: Object s SVG -> Scene s ()
applyScene instanceHighlight = do
  -- Highlight the apply method
  oModifyS instanceHighlight $ oContext .= withFillColor "blue"
  wait 1

  -- Show apply function
  oApplyFunction <- mapM oNew applyFunctionGlyphs
  forM_ oApplyFunction oShow

  wait 2

  let toMoveApply = drop 4 oApplyFunction
      restApply = take 4 oApplyFunction

  oApplyRedex <- mapM oNew applyRedexGlyphs

  let toShowRedex = head oApplyRedex
      toGetCoordsRedex = tail oApplyRedex

  newRedexCoordsX <- mapM (`oRead` oLeftX) toGetCoordsRedex

  -- Show the redex
  forM_ (zip toMoveApply newRedexCoordsX) $ \(o, x) ->
    fork $ oTweenS o 1 $ \t -> oLeftX %= \prev -> fromToS prev x t

  forM_ restApply (fork . (`oHideWith` oFadeOut))
  fork $ oShowWith toShowRedex oFadeIn

  wait 2

  -- Show result
  oApplyResult <- mapM oNew applyResultGlyphs

  let toMoveResult = drop 4 toMoveApply
      toShowResult = take 2 oApplyResult
      toGetCoordsResult = drop 2 oApplyResult
      restResult = toShowRedex:take 4 toMoveApply

  newResultCoordsX <- mapM (`oRead` oLeftX) toGetCoordsResult

  forM_ (zip toMoveResult newResultCoordsX) $ \(o, x) ->
    fork $ oTweenS o 1 $ \t -> oLeftX %= \prev -> fromToS prev x t

  forM_ toShowResult (fork . (`oShowWith` oFadeIn))
  forM_ restResult (fork . (`oHideWith` oFadeOut))

  wait 2

  oModifyS instanceHighlight $ oContext .= withFillColor "black"
  forM_ (toMoveResult ++ toShowResult) (fork . (`oHideWith` oFadeOut))

  wait 1

liftA2Scene :: Object s SVG -> Scene s ()
liftA2Scene instanceHighlight = do
  -- Highlight the apply method
  oModifyS instanceHighlight $ oContext .= withFillColor "blue"
  wait 1

  -- Show liftA2 function
  oLiftA2Function <- mapM oNew liftA2Glyphs
  forM_ oLiftA2Function (fork . (`oShowWith` oFadeIn))

  wait 2

  let toMoveFunction = take 7 oLiftA2Function
      restFunction = drop 7 oLiftA2Function

  -- Show regex
  oLA2Redex <- mapM oNew lA2RedexGlyphs

  let toShowRedex = take 2 oLA2Redex
      toGetCoords = drop 2 oLA2Redex

  newXCoordsRedex <- mapM (`oRead` oLeftX) toGetCoords

  forM_ (zip toMoveFunction newXCoordsRedex) $ \(o, x) ->
    fork $ oTweenS o 1 $ \t -> oLeftX %= \prev -> fromToS prev x t


  forM_ toShowRedex (fork . (`oShowWith` oFadeIn))
  forM_ restFunction (fork . (`oHideWith` oFadeOut))

  wait 2

  -- Show result
  oLA2Result <- mapM oNew lA2ResultGlyphs
  let toShowResult = take 2 oLA2Result
      toMoveResult = take 3 toMoveFunction
      toHide = toShowRedex ++ drop 3 toMoveFunction
      toGetCoordsResult = drop 2 oLA2Result

  newXCoordsResult <- mapM (`oRead` oLeftX) toGetCoordsResult

  forM_ (zip toMoveResult newXCoordsResult) $ \(o, x) ->
    fork $ oTweenS o 1 $ \t -> oLeftX %= \prev -> fromToS prev x t

  forM_ toShowResult (fork . (`oShowWith` oFadeIn))
  forM_ toHide (fork . (`oHideWith` oFadeOut))

  wait 2
