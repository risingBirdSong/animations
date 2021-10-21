module Monad.Maybe.Animation where

import Reanimate ( Animation, scene )
import Monad.Maybe.Common ( env, showClass )
import qualified Monad.Maybe.Explicit.Scene as Explicit (fullScene)
import qualified Monad.Maybe.Sugared.Scene as Sugared (fullScene)

animation :: Animation
animation = env $ do
  scene $ do
    showClass
    Explicit.fullScene
    Sugared.fullScene
