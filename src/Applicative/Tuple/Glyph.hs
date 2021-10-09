{-# LANGUAGE OverloadedStrings #-}

module Applicative.Tuple.Glyph where

import Functor.Common ( scaleFactor )
import Reanimate
    ( SVG,
      Animation,
      addStatic,
      latex,
      splitGlyphs,
      svgGlyphs,
      center,
      mkBackground,
      scale )

bg :: SVG
bg = mkBackground "lightblue"

env :: Animation -> Animation
env = addStatic bg

tupleInstanceHeader :: SVG
tupleInstanceHeader =
  scale scaleFactor $ latex "instance Monoid a $\\Rightarrow$ Applicative ((,) a) where"

pureTuple :: SVG
pureTuple =
  scale scaleFactor $ latex "pure x = (mempty, x)"

applyTuple :: SVG
applyTuple =
  scale scaleFactor $ latex "(u, f) \\textless*\\textgreater (v, x) = (u \\textless\\textgreater v, f x)"

liftA2Tuple :: SVG
liftA2Tuple =
  scale scaleFactor $ latex "liftA2 f (u, x) (v, y) = (u \\textless\\textgreater v, f x y)"

pureFunction :: SVG
pureFunction = center $ latex "pure 4 :: (String, Integer)"

pureArg :: SVG
pureArg =
  snd $
    splitGlyphs [4] pureFunction

purePrev :: SVG
purePrev =
  snd $ splitGlyphs [0..3] pureFunction

pureRest :: SVG
pureRest =
  snd $ splitGlyphs [5..length (svgGlyphs pureFunction)] pureFunction

pureFunctionGlyphs :: [SVG]
pureFunctionGlyphs = [purePrev, pureArg, pureRest]

pureMempty :: SVG
pureMempty =
  center $ latex "(mempty :: String, 4)"

pureMemptyArg :: SVG
pureMemptyArg =
  snd $ splitGlyphs [16] pureMempty

pureMemptyCParen :: SVG
pureMemptyCParen =
  snd $
    splitGlyphs [17] pureMempty

pureMemptyOParen :: SVG
pureMemptyOParen =
  snd $
    splitGlyphs [0] pureMempty

pureMemptyComma :: SVG
pureMemptyComma =
  snd $ splitGlyphs [15] pureMempty

pureMemptyRest :: SVG
pureMemptyRest =
  snd $ splitGlyphs [1..14] pureMempty

pureMemptyGlyphs :: [SVG]
pureMemptyGlyphs = [pureMemptyOParen, pureMemptyRest, pureMemptyComma, pureMemptyCParen]

pureResult :: SVG
pureResult = center $ latex "(\"\", 4)"

pureResultOParen :: SVG
pureResultOParen =
  snd $
    splitGlyphs [0] pureResult

pureResultFst :: SVG
pureResultFst =
  snd $
    splitGlyphs [1,2] pureResult

pureResultComma :: SVG
pureResultComma =
  snd $
    splitGlyphs [3] pureResult

pureResultArg :: SVG
pureResultArg =
  snd $
    splitGlyphs [4] pureResult

pureResultCParen :: SVG
pureResultCParen =
  snd $
    splitGlyphs [5] pureResult

pureResultGlyphs :: [SVG]
pureResultGlyphs = [pureResultOParen, pureResultFst, pureResultComma, pureResultArg, pureResultCParen]

applyFunction :: SVG
applyFunction = center $ latex "(\"hello \", (+ 2)) \\textless*\\textgreater (\"there\", 4)"

applyOParen :: SVG
applyOParen =
  snd $
    splitGlyphs [0] applyFunction

applyFst :: SVG
applyFst =
  snd $
    splitGlyphs [1..7] applyFunction

applyComma :: SVG
applyComma =
  snd $
    splitGlyphs [8] applyFunction

applySnd :: SVG
applySnd =
  snd $
    splitGlyphs [9..12] applyFunction

applyCParenOne :: SVG
applyCParenOne =
  snd $ splitGlyphs [13] applyFunction

applySymbol :: SVG
applySymbol =
  snd $ splitGlyphs [14..16] applyFunction

applyOParen' :: SVG
applyOParen' =
  snd $ splitGlyphs [17] applyFunction

applyFst' :: SVG
applyFst' =
  snd $ splitGlyphs [18..24] applyFunction

applyComma' :: SVG
applyComma' =
  snd $ splitGlyphs [25] applyFunction

applySnd' :: SVG
applySnd' =
  snd $ splitGlyphs [26] applyFunction

applyCParen :: SVG
applyCParen =
  snd $ splitGlyphs [27] applyFunction

applyFunctionGlyphs :: [SVG]
applyFunctionGlyphs =
  [ -- To show & drop
    applySymbol
  , applyOParen'
  , applyComma'
  , applyCParenOne
  -- To keep
  , applyFst
  , applySnd
  , applyFst'
  , applySnd'
  -- To keep at the end
  , applyOParen
  , applyCParen
  , applyComma
  ]

applyRedex :: SVG
applyRedex = center $ latex "(\"hello \" \\textless\\textgreater \"there\", (+2) 4)"

applyRedexOParen :: SVG
applyRedexOParen =
  snd $ splitGlyphs [0] applyRedex

applyRedexFst :: SVG
applyRedexFst =
  snd $ splitGlyphs [1..7] applyRedex

applyRedexMappend :: SVG
applyRedexMappend =
  snd $ splitGlyphs [8,9] applyRedex

applyRedexSnd :: SVG
applyRedexSnd =
  snd $ splitGlyphs [10..16] applyRedex

applyRedexComma :: SVG
applyRedexComma =
  snd $ splitGlyphs [17] applyRedex

applyRedexFst' :: SVG
applyRedexFst' =
  snd $ splitGlyphs [18..21] applyRedex

applyRedexSnd' :: SVG
applyRedexSnd' =
  snd $ splitGlyphs [22] applyRedex

applyRedexCParen :: SVG
applyRedexCParen =
  snd $ splitGlyphs [23] applyRedex

applyRedexGlyphs :: [SVG]
applyRedexGlyphs =
  [ -- To show & drop
    applyRedexMappend
    -- To get Coordinates from
  , applyRedexFst
  , applyRedexFst'
  , applyRedexSnd
  , applyRedexSnd'
  , applyRedexOParen
  , applyRedexCParen
  , applyRedexComma
  ]

applyResult :: SVG
applyResult = center $ latex "(\"hello there\", 6)"

applyResultOParen :: SVG
applyResultOParen =
  snd $
    splitGlyphs [0] applyResult

applyResultFst :: SVG
applyResultFst =
  snd $
    splitGlyphs [1..12] applyResult

applyResultComma :: SVG
applyResultComma =
  snd $
    splitGlyphs [13] applyResult

applyResultSnd :: SVG
applyResultSnd =
  snd $
    splitGlyphs [14] applyResult

applyResultCParen :: SVG
applyResultCParen =
  snd $
    splitGlyphs [15] applyResult

applyResultGlyphs :: [SVG]
applyResultGlyphs =
  [ -- To show
    applyResultFst
  , applyResultSnd
  -- To get coordinates
  , applyResultOParen
  , applyResultCParen
  , applyResultComma
  ]

liftA2TupleFunction :: SVG
liftA2TupleFunction =
  center $ scale 0.7 $ latex "liftA2 (+) (\"General \", 4) (\"Kenobi\", 4)"

lA2 :: SVG
lA2 =
  snd $ splitGlyphs [0..5] liftA2TupleFunction

lF :: SVG
lF =
  snd $ splitGlyphs [6..8] liftA2TupleFunction

lOParen :: SVG
lOParen =
  snd $ splitGlyphs [9] liftA2TupleFunction

lFst :: SVG
lFst =
  snd $ splitGlyphs [10..18] liftA2TupleFunction

lComma :: SVG
lComma =
  snd $ splitGlyphs [19] liftA2TupleFunction

lSnd :: SVG
lSnd =
  snd $ splitGlyphs [20] liftA2TupleFunction

lCParen :: SVG
lCParen =
  snd $ splitGlyphs [21] liftA2TupleFunction

lOParen' :: SVG
lOParen' =
  snd $ splitGlyphs [22] liftA2TupleFunction

lFst' :: SVG
lFst' =
  snd $ splitGlyphs [23..30] liftA2TupleFunction

lComma' :: SVG
lComma' =
  snd $ splitGlyphs [31] liftA2TupleFunction

lSnd' :: SVG
lSnd' =
  snd $ splitGlyphs [32] liftA2TupleFunction

lCParen' :: SVG
lCParen' =
  snd $ splitGlyphs [33] liftA2TupleFunction

liftA2Glyphs :: [SVG]
liftA2Glyphs =
  [ -- To move & drop
    lOParen
  , lComma
  , lCParen'
  , lFst
  , lSnd
  , lFst'
  , lSnd'
  -- Rest
  , lF
  , lFst
  , lComma
  , lSnd
  , lCParen
  , lOParen'
  , lFst'
  , lComma'
  , lSnd'
  ]

lA2Redex :: SVG
lA2Redex =
  center $ scale 0.7 $ latex "(\"General \" \\textless\\textgreater \"Kenobi\", (+) 4 4)"

lrOParen :: SVG
lrOParen =
  snd $ splitGlyphs [0] lA2Redex

lrFst :: SVG
lrFst =
  snd $ splitGlyphs [1..9] lA2Redex

lrMappend :: SVG
lrMappend =
  snd $ splitGlyphs [10,11] lA2Redex

lrFst' :: SVG
lrFst' =
  snd $ splitGlyphs [12..19] lA2Redex

lrComma :: SVG
lrComma =
  snd $ splitGlyphs [20] lA2Redex

lrF :: SVG
lrF =
  snd $ splitGlyphs [21..23] lA2Redex

lrSnd :: SVG
lrSnd =
  snd $ splitGlyphs [24] lA2Redex

lrSnd' :: SVG
lrSnd' =
  snd $ splitGlyphs [25] lA2Redex

lrCParen' :: SVG
lrCParen' =
  snd $ splitGlyphs [26] lA2Redex

lA2RedexGlyphs :: [SVG]
lA2RedexGlyphs =
  [ -- To show & drop
    lrMappend
  , lrF
  -- To get coordinates
  , lrOParen
  , lrComma
  , lrCParen'
  , lrFst
  , lrSnd
  , lrFst'
  , lrSnd'
  ]

lA2Result :: SVG
lA2Result =
  center $ scale 0.7 $ latex "(\"General Kenobi\", 8)"

larOParen :: SVG
larOParen =
  snd $ splitGlyphs [0] lA2Result

larFst :: SVG
larFst =
  snd $ splitGlyphs [1..15] lA2Result

larComma :: SVG
larComma =
  snd $ splitGlyphs [16] lA2Result

larSnd :: SVG
larSnd =
  snd $ splitGlyphs [17] lA2Result

larCParen :: SVG
larCParen =
  snd $ splitGlyphs [18] lA2Result

lA2ResultGlyphs :: [SVG]
lA2ResultGlyphs =
  [ -- To show
    larFst
  , larSnd
  -- To get the coordinates
  , larOParen
  , larComma
  , larCParen
  ]
