module Random where

import Kleisli
import Prelude

nextRandom :: Double -> Double
nextRandom = snd . properFraction . (105.947 *)

type Random a = State Double a

next :: Random Double
next = State $ \s -> (s, nextRandom s)

addRandom :: Double -> Random Double
addRandom x = (+x) +$ next
