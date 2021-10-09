module Applicative.Tuple.Animation where

import Reanimate ( Animation, scene, wait )
import Applicative.Tuple.Glyph ( env )
import Applicative.Tuple.Scene
    ( showClass, pureScene, applyScene, liftA2Scene )
import Common ( mkClass, moveTopLeft )
import Reanimate.Scene ( oNew )
import Applicative.Common
    ( applySignature,
      baseX,
      classHeader,
      liftA2Signature,
      pureSignature )

animation :: Animation
animation = env $
  scene $ do
    oApplicativeClass <- oNew $ mkClass baseX classHeader [ pureSignature
                                                          , applySignature
                                                          , liftA2Signature
                                                          ]
    showClass oApplicativeClass
    moveTopLeft 0.6 oApplicativeClass
    wait 1
    oInstanceParts <- pureScene oApplicativeClass
    let [_, _, oApplyInstance, oLiftA2Instance] = oInstanceParts
    applyScene oApplyInstance
    wait 1
    liftA2Scene oLiftA2Instance
    wait 1
